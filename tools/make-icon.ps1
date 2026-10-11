$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.Drawing
$destination = Join-Path (Split-Path -Parent $PSScriptRoot) 'assets/icon'
New-Item -ItemType Directory -Force -Path $destination | Out-Null
$svg = @'
<svg xmlns="http://www.w3.org/2000/svg" width="256" height="256" viewBox="0 0 256 256">
  <rect x="8" y="8" width="240" height="240" rx="54" fill="#203D37"/>
  <path d="M68 184V103a60 60 0 0 1 120 0v81" fill="none" stroke="#D4DBC4" stroke-width="14" stroke-linecap="round"/>
  <path d="M92 136c28-18 48-18 72 0" fill="none" stroke="#98B7A2" stroke-width="10" stroke-linecap="round"/>
  <path d="M92 163h72M106 184h44" fill="none" stroke="#98B7A2" stroke-width="8" stroke-linecap="round"/>
</svg>
'@
[IO.File]::WriteAllText((Join-Path $destination 'stillroom.svg'), $svg)
$canvas = [Drawing.Bitmap]::new(1024,1024)
$graphics = [Drawing.Graphics]::FromImage($canvas)
$graphics.SmoothingMode = [Drawing.Drawing2D.SmoothingMode]::AntiAlias
$graphics.ScaleTransform(4,4)
$graphics.Clear([Drawing.Color]::Transparent)
$tile = [Drawing.Drawing2D.GraphicsPath]::new()
$tile.AddArc(8,8,108,108,180,90)
$tile.AddArc(140,8,108,108,270,90)
$tile.AddArc(140,140,108,108,0,90)
$tile.AddArc(8,140,108,108,90,90)
$tile.CloseFigure()
$brush = [Drawing.SolidBrush]::new([Drawing.ColorTranslator]::FromHtml('#203D37'))
$graphics.FillPath($brush,$tile)
$arch = [Drawing.Drawing2D.GraphicsPath]::new()
$arch.AddLine(68,184,68,103)
$arch.AddArc(68,43,120,120,180,180)
$arch.AddLine(188,103,188,184)
$pen = [Drawing.Pen]::new([Drawing.ColorTranslator]::FromHtml('#D4DBC4'),14)
$pen.StartCap = $pen.EndCap = [Drawing.Drawing2D.LineCap]::Round
$graphics.DrawPath($pen,$arch)
$pen.Color = [Drawing.ColorTranslator]::FromHtml('#98B7A2')
$pen.Width = 10
$ridge = [Drawing.Drawing2D.GraphicsPath]::new()
$ridge.AddBezier(92,136,120,118,140,118,164,136)
$graphics.DrawPath($pen,$ridge)
$pen.Width = 8
$graphics.DrawLine($pen,92,163,164,163)
$graphics.DrawLine($pen,106,184,150,184)
$graphics.Dispose()
$frames = @()
foreach ($size in @(16,24,32,48,64,128,256)) {
    $bitmap = [Drawing.Bitmap]::new($size,$size)
    $drawing = [Drawing.Graphics]::FromImage($bitmap)
    $drawing.InterpolationMode = [Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
    $drawing.DrawImage($canvas,0,0,$size,$size)
    $drawing.Dispose()
    $stream = [IO.MemoryStream]::new()
    $bitmap.Save($stream,[Drawing.Imaging.ImageFormat]::Png)
    $frames += [pscustomobject]@{Size=$size; Bytes=$stream.ToArray()}
    if ($size -eq 256) { $bitmap.Save((Join-Path $destination 'stillroom.png'),[Drawing.Imaging.ImageFormat]::Png) }
    $stream.Dispose(); $bitmap.Dispose()
}
$file = [IO.File]::Create((Join-Path $destination 'stillroom.ico'))
$writer = [IO.BinaryWriter]::new($file)
$writer.Write([uint16]0); $writer.Write([uint16]1); $writer.Write([uint16]$frames.Count)
$offset = 6 + 16 * $frames.Count
foreach ($frame in $frames) {
    $byteSize = if ($frame.Size -eq 256) { 0 } else { $frame.Size }
    $writer.Write([byte]$byteSize); $writer.Write([byte]$byteSize)
    $writer.Write([byte]0); $writer.Write([byte]0)
    $writer.Write([uint16]1); $writer.Write([uint16]32)
    $writer.Write([uint32]$frame.Bytes.Length); $writer.Write([uint32]$offset)
    $offset += $frame.Bytes.Length
}
foreach ($frame in $frames) { $writer.Write([byte[]]$frame.Bytes) }
$writer.Dispose(); $file.Dispose()
$canvas.Dispose(); $tile.Dispose(); $arch.Dispose(); $ridge.Dispose(); $pen.Dispose(); $brush.Dispose()
Get-ChildItem -LiteralPath $destination | Select-Object Name,Length
