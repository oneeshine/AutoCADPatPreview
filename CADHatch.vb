'Copyright ©️ 2023-2024 litestudio 福萝卜 All rights reserved.

'CONTACT
' - email: oneeshine@163.com
' - qq: 461884072

'MIT License (MIT)

'Permission Is hereby granted, free Of charge, to any person obtaining a copy of
'this software And associated documentation files (the "Software"), To deal In
'the Software without restriction, including without limitation the rights To
'use, copy, modify, merge, publish, distribute, sublicense, And/Or sell copies of
'the Software, and To permit persons To whom the Software Is furnished To Do so,
'subject To the following conditions:

'The above copyright notice And this permission notice shall be included In all
'copies Or substantial portions of the Software.

'THE SOFTWARE Is PROVIDED "AS IS", WITHOUT WARRANTY Of ANY KIND, EXPRESS Or
'IMPLIED, INCLUDING BUT Not LIMITED To THE WARRANTIES Of MERCHANTABILITY, FITNESS
'For A PARTICULAR PURPOSE And NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS Or
'COPYRIGHT HOLDERS BE LIABLE For ANY CLAIM, DAMAGES Or OTHER LIABILITY, WHETHER
'IN AN ACTION OF CONTRACT, TORT Or OTHERWISE, ARISING FROM, OUT OF Or IN
'CONNECTION WITH THE SOFTWARE Or THE USE Or OTHER DEALINGS IN THE SOFTWARE.


Imports AutoCADPatViewer.CADHatch.PatLine
Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports System.Text.RegularExpressions
Imports System.Threading
''' <summary>
''' CAD系统填充 PatternHath
''' </summary>
Public Class CADHatch
  Implements IDisposable

  Private _Size As Integer = 400
  ''' <summary>
  ''' 要获取的缩略图尺寸
  ''' </summary>
  ''' <returns></returns>
  Public Property Size() As Integer
    Get
      Return _Size
    End Get
    Set(ByVal value As Integer)
      If value <> _Size Then
        _Size = value

      End If
    End Set
  End Property

  ''' <summary>
  ''' 填充名称
  ''' </summary>
  ''' <returns></returns>
  Public Property RealName As String

  Public Property HighlightStart As Integer = -1
  Public Property HighlightEnd As Integer = -1

  Public Property HighlightColor As Color = Color.Red

  Public Property HighlightCurrentLine As Boolean

  ''' <summary>
  ''' 实例化此类，读取SnapBitmapImage属性即可获得预览图
  ''' </summary>
  ''' <param name="Define">pat定义内容</param>
  ''' <param name="initSnap">是否生成预览图</param>
  ''' <param name="HighlightStart">高亮起始行</param>
  ''' <param name="HighlightEnd">高亮结束行</param>
  ''' <param name="HighlightColor">高亮颜色</param>
  ''' <param name="HighlightCurrentLine">高亮指定的行</param>
  ''' <param name="Size">预览图尺寸</param>
  ''' <param name="PreviewScale">预览图比例，如果是0，会自动计算合适的比例</param>
  Sub New(ByVal Define As String, initSnap As Boolean, HighlightStart As Integer, HighlightEnd As Integer, HighlightColor As Color, HighlightCurrentLine As Boolean, Optional ByVal Size As Integer = 400, Optional ByVal PreviewScale As Double = 0)
    Me.HighlightStart = HighlightStart
    Me.HighlightEnd = HighlightEnd
    Me.HighlightColor = HighlightColor
    Me.HighlightCurrentLine = HighlightCurrentLine
    Me.Size = Size
    Me.PreviewScale = PreviewScale
    Dim DL() As String = Define.Split({vbCrLf, vbLf, vbCr}, StringSplitOptions.None)
    Init(DL， initSnap)
  End Sub

  ''' <summary>
  ''' 缩放，会重新生成预览图
  ''' </summary>
  ''' <param name="PreviewScale">必须大于0</param>
  Friend Sub ReScale(ByVal PreviewScale As Double)
    If PreviewScale <= 0 Then
      Exit Sub
    End If
    Image_Private?.Dispose()
    Image_Private = Nothing
    SnapBitmapImage = Nothing

    Me.PreviewScale = PreviewScale
    LoadSnap()
  End Sub

  ''' <summary>
  ''' 用定义初始化Pattern填充
  ''' </summary>
  ''' <param name="DefineLines"></param>
  ''' <param name="initSnap">是否生成预览图</param>
  Private Sub Init(ByVal DefineLines As String()， initSnap As Boolean)

    Define = DefineLines
    Try
      IsValid = True
      If DefineLines.Count > 1 Then
        Dim TitleLine As String = DefineLines(0)

        Dim Titelregex As New Regex("^\*.+,{0,1}[^,]*")
        If Titelregex.IsMatch(TitleLine) Then

          Dim DotIndex As Integer = TitleLine.IndexOf(",")

          If DotIndex > 0 Then
            RealName = TitleLine.Substring(1, DotIndex - 1)
          Else
            RealName = TitleLine.Remove(0, 1)
          End If


          If RealName.Length > 31 Then '名称不能大于31个字符
            IsValid = False
          Else

            For i = 0 To DefineLines.Length - 1

              Dim PatLine As New PatLine()
              LineArgs.Add(PatLine)

              Dim TS As String = DefineLines(i)

              Dim SS As String() = TS.Split(",")

              If SS.Length Mod 2 = 0 Then
                Dim TSL As New List(Of String)(SS)
                TSL.Add(0)
                SS = TSL.ToArray
              End If

              If SS.Length >= 5 Then

                For Each S As String In SS
                  S = S.Trim(" ")
                  If Not IsNumeric(S) Then
                    IsValid = False
                    Exit For
                  End If

                Next

                If Not IsValid Then
                  Continue For
                End If

                PatLine.PutDefine(TS)

                PatLine.Angle = CDbl(SS(0))
                PatLine.Origin = New PointF(CSng(SS(1)), CSng(SS(2)))
                PatLine.Offset = New PointF(CSng(SS(3)), CSng(SS(4)))



                Dim TotalDashLineLength As Double = 0
                Dim X As Double = 0
                For j = 5 To SS.Length - 1
                  Dim L As Double = CDbl(SS(j))
                  Dim Len As Double = Math.Abs(L)
                  TotalDashLineLength += Len
                  PatLine.DashLineList.Add(New DashLine(X, L < 0, Len))

                  X += Len
                Next
                PatLine.TotalDashLineLength = TotalDashLineLength

                If PreviewScale = 0 Then
                  PreviewScale = Math.Max(Math.Max(Size / 3 / Math.Max(Math.Abs(PatLine.Origin.X), Math.Max(Math.Abs(PatLine.Origin.Y), Math.Max(Math.Abs(PatLine.Offset.Y), Math.Abs(PatLine.Offset.X)))), 0.1), PreviewScale)
                  PreviewScale = Math.Min(Size, PreviewScale)
                End If

              Else
                Continue For
              End If
              If Not IsValid Then
                Continue For
              End If
            Next
          End If

        Else
          IsValid = False
        End If
      Else
        IsValid = False
      End If

      If IsValid AndAlso initSnap Then
        LoadSnap()
      End If


    Catch ex As Exception

    End Try


  End Sub


  Private LoadThread As Thread

  ''' <summary>
  ''' 加载缩略图
  ''' </summary>
  Private Sub LoadSnap()

    If SnapBitmapImage Is Nothing Then
      Dim Graphics As Graphics = Graphics.FromHwnd(IntPtr.Zero)
      SnapBitmapImage = New WriteableBitmap(Size, Size, Graphics.DpiX, Graphics.DpiY, PixelFormats.Pbgra32, Nothing)
      LoadThread = New Thread(AddressOf LoadSnapSub) With {.IsBackground = True}
      LoadThread.Start(New With {.BackBuffer = SnapBitmapImage.BackBuffer, .BackBufferStride = SnapBitmapImage.BackBufferStride})
    End If

  End Sub

  ''' <summary>
  ''' 
  ''' </summary>
  Private Sub LoadSnapSub(ob As Object)
    Try


      Dim BackBuffer As IntPtr = ob.BackBuffer
      Dim BackBufferStride As Integer = ob.BackBufferStride

      SnapBitmapImage.Dispatcher.Invoke(Sub()
                                          _SnapBitmapImage.Lock()
                                        End Sub)
      Try


        Dim FrontSnap As Bitmap = PatternSnapBitmap()
        Dim Bmap As New Bitmap(Size, Size, BackBufferStride, System.Drawing.Imaging.PixelFormat.Format32bppPArgb, BackBuffer)
        Dim G As Graphics = Graphics.FromImage(Bmap)
        G.Clear(Color.White)
        If FrontSnap IsNot Nothing Then
          G.DrawImage(FrontSnap, 0, 0, Bmap.Width, Bmap.Height)
        End If
        G.Dispose()
        Bmap.Dispose()

      Catch ex As Exception

      End Try

      SnapBitmapImage.Dispatcher.Invoke(Sub()
                                          SnapBitmapImage.AddDirtyRect(New Int32Rect(0, 0, Size, Size))
                                          SnapBitmapImage.Unlock()
                                          'RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs("SnapBitmapImage"))
                                        End Sub)
    Catch ex As Exception

    End Try
  End Sub

  ''' <summary>
  ''' 计算缩略图时的比例
  ''' </summary>
  ''' <returns></returns>
  Friend Property PreviewScale As Double = 0

  ''' <summary>
  ''' 线定义集合
  ''' </summary>
  Private LineArgs As New List(Of PatLine)


  ''' <summary>
  ''' 定义
  ''' </summary>
  Private Define As String()
  ''' <summary>
  ''' pat是否有效
  ''' </summary>
  ''' <returns></returns>
  Public Property IsValid As Boolean


  Private _SnapBitmapImage As WriteableBitmap = Nothing
  ''' <summary>
  ''' Pattern预览图，WriteableBitmap格式
  ''' </summary>
  ''' <returns></returns>
  Public Property SnapBitmapImage As WriteableBitmap
    Get
      Return _SnapBitmapImage
    End Get
    Set(value As WriteableBitmap)
      _SnapBitmapImage = value
    End Set
  End Property


  Private Image_Private As Bitmap = Nothing
  Private disposedValue As Boolean

  ''' <summary>
  ''' Pattern预览图,bitmap格式 
  ''' </summary>
  ''' <returns></returns>
  Friend Function PatternSnapBitmap() As Bitmap

    If Image_Private Is Nothing Then

      Image_Private = New Bitmap(Size, Size)

      Dim G As Graphics = Graphics.FromImage(Image_Private)
      G.Clear(Color.White)
      Dim Pen As New Pen(Color.Black)

      Try

        G.CompositingQuality = CompositingQuality.HighSpeed

        G.Clear(Color.White)
        If PreviewScale <> 0 Then

          Dim Range As Integer = Size

          G.DrawRectangle(Pen, 0, 0, Size - 1, Size - 1)

          For u As Integer = 0 To LineArgs.Count - 1


            Dim PatLine As PatLine = LineArgs(u)

            If PatLine.IsInValid Then Continue For

            If HighlightCurrentLine AndAlso u >= HighlightStart AndAlso u <= HighlightEnd Then
              Pen.Color = HighlightColor
            Else
              Pen.Color = System.Drawing.Color.Black
            End If


            G.ResetTransform()
            G.TranslateTransform(Size / 2, Size / 2)


            G.TranslateTransform(PatLine.Origin.X * PreviewScale, PatLine.Origin.Y * PreviewScale)
            G.RotateTransform(PatLine.Angle)

            If PatLine.Dashed Then


              For i = 0 To PatLine.DashLineList.Count - 1

                Dim DashLine As DashLine = PatLine.DashLineList(i)
                If Not DashLine.Neg Then

                  Dim repeat As Integer


                  Dim LinePtX As New List(Of Single)

                  For repeat = 0 To Integer.MaxValue

                    Dim ST As PointF = New PointF(DashLine.StartX * PreviewScale + PatLine.TotalDashLineLength * repeat * PreviewScale, 0)
                    Dim ET As PointF = ST.Add(New Vector(Math.Max(1, DashLine.Length * PreviewScale), 0))

                    G.DrawLine(Pen, ST, ET)

                    LinePtX.Add(ST.X)
                    LinePtX.Add(ET.X)
                    If Math.Abs(ST.X) > Range OrElse Math.Abs(ST.Y) > Range Then
                      Exit For
                    End If
                  Next


                  For repeat0 = repeat To 0 Step -1

                    Dim ST As PointF = New PointF(DashLine.EndX * PreviewScale - PatLine.TotalDashLineLength * repeat0 * PreviewScale, 0)
                    Dim ET As PointF = ST.Add(New Vector(-Math.Max(1, DashLine.Length * PreviewScale), 0))

                    G.DrawLine(Pen, ST, ET)

                    LinePtX.Insert(0, ST.X)
                    LinePtX.Insert(0, ET.X)

                  Next


                  For offset As Integer = 0 To Integer.MaxValue

                    Dim YOffset As Double = offset * PatLine.Offset.Y * PreviewScale
                    Dim XOffset As Double = ((offset * PatLine.Offset.X) Mod PatLine.TotalDashLineLength) * PreviewScale
                    Dim XOffset0 As Double = ((offset * (PatLine.Offset.X + PatLine.TotalDashLineLength)) Mod PatLine.TotalDashLineLength) * PreviewScale
                    For c = 0 To LinePtX.Count - 1 Step 2

                      Dim ST As PointF = New PointF(LinePtX(c) + XOffset, YOffset)
                      Dim ET As PointF = New PointF(LinePtX(c + 1) + XOffset, YOffset)

                      Dim Dx As Single = ET.X - ST.X
                      If Dx >= 0 Then
                        If Dx < 1 Then
                          ET.X = ST.X + 1
                        End If
                      Else
                        If Dx > -1 Then
                          ET.X = ST.X - 1
                        End If
                      End If

                      G.DrawLine(Pen, ST, ET)

                      ST = New PointF(LinePtX(c) - XOffset0, -YOffset)
                      ET = New PointF(LinePtX(c + 1) - XOffset0, -YOffset)

                      Dx = ET.X - ST.X
                      If Dx >= 0 Then
                        If Dx < 1 Then
                          ET.X = ST.X + 1
                        End If
                      Else
                        If Dx > -1 Then
                          ET.X = ST.X - 1
                        End If
                      End If

                      G.DrawLine(Pen, ST, ET)
                    Next

                    If Math.Abs(YOffset) > Range Then
                      Exit For
                    End If
                  Next


                End If


              Next

            Else

              For offset As Integer = 0 To Integer.MaxValue
                Dim YOffset As Double = offset * PatLine.Offset.Y * PreviewScale
                Dim ST As PointF = New PointF(0, 0).Add(New Vector(-Size, YOffset))
                Dim ET As PointF = New PointF(0, 0).Add(New Vector(Size, YOffset))
                G.DrawLine(Pen, ST, ET)

                ST = New PointF(0, 0).Add(New Vector(-Size, -YOffset))
                ET = New PointF(0, 0).Add(New Vector(Size, -YOffset))

                G.DrawLine(Pen, ST, ET)

                If Math.Abs(YOffset) > Range Then
                  Exit For

                End If
              Next


            End If


          Next


          Image_Private.RotateFlip(RotateFlipType.RotateNoneFlipY)

          'Image_Private.MakeTransparent(Color.White)
        End If

        G.Dispose()

      Catch ex As Exception
        G.Clear(Color.Transparent)
      End Try
    End If
    Return Image_Private

  End Function



  ''' <summary>
  ''' 填充线定义
  ''' </summary>
  Public Class PatLine

    ''' <summary>
    ''' 默认为无效的
    ''' </summary>
    Sub New()

    End Sub

    Sub PutDefine(PatDefineStr As String)
      Me.PatDefineStr = PatDefineStr
      IsInValid = False
    End Sub

    ''' <summary>
    ''' 无效的
    ''' </summary>
    ''' <returns></returns>
    Public Property IsInValid As Boolean = True

    Public Property PatDefineStr As String = ""

    ''' <summary>
    ''' 角度值
    ''' </summary>
    ''' <returns></returns>
    Public Property Angle As Double

    Public Property Origin As PointF = New PointF(0F, 0F)



    Public Property Offset As PointF = New PointF(0F, 0F)


    Public Property DashLineList As New List(Of DashLine)


    Public Property TotalDashLineLength As Double
    ''' <summary>
    ''' 是否虚线
    ''' </summary>
    ''' <returns></returns>
    Public ReadOnly Property Dashed As Boolean
      Get
        Return DashLineList.Count <> 0
      End Get
    End Property


    ''' <summary>
    ''' 虚线类
    ''' </summary>
    Public Class DashLine
      Property StartX As Double

      Property Neg As Boolean

      Property Length As Double

      Property EndX As Double


      Sub New(StartX As Double, Neg As Boolean, Length As Double)
        Me.StartX = StartX
        Me.Neg = Neg
        Me.Length = Length
        Me.EndX = StartX + Length
      End Sub
    End Class
  End Class

  Protected Overridable Sub Dispose(disposing As Boolean)
    If Not disposedValue Then
      If disposing Then
        ' TODO: 释放托管状态(托管对象)
      End If

      LoadThread?.Abort()
      SnapBitmapImage = Nothing
      Image_Private?.Dispose()
      ' TODO: 释放未托管的资源(未托管的对象)并重写终结器
      ' TODO: 将大型字段设置为 null
      disposedValue = True
    End If
  End Sub

  ' ' TODO: 仅当“Dispose(disposing As Boolean)”拥有用于释放未托管资源的代码时才替代终结器
  ' Protected Overrides Sub Finalize()
  '     ' 不要更改此代码。请将清理代码放入“Dispose(disposing As Boolean)”方法中
  '     Dispose(disposing:=False)
  '     MyBase.Finalize()
  ' End Sub

  Public Sub Dispose() Implements IDisposable.Dispose
    ' 不要更改此代码。请将清理代码放入“Dispose(disposing As Boolean)”方法中
    Dispose(disposing:=True)
    GC.SuppressFinalize(Me)
  End Sub
End Class

