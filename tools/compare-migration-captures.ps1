param(
    [Parameter(Mandatory=$true)][string]$BaselineDirectory,
    [Parameter(Mandatory=$true)][string]$ActualDirectory,
    [Parameter(Mandatory=$true)][string]$OutputFile,
    [ValidateSet('quality-capture','capture-ui','check-journey','check-graphics')][string[]]$Modes = @('quality-capture','capture-ui','check-journey','check-graphics')
)
$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.Drawing
if (-not ('StillroomCaptureDifference' -as [type])) {
    $references = @('System.Drawing.Common','System.Drawing.Primitives','System.Runtime.InteropServices')
    foreach ($optional in @('System.Private.Windows.GdiPlus','System.Private.Windows.Core')) {
        try { $references += [Reflection.Assembly]::Load($optional).Location } catch { }
    }
    Add-Type -ReferencedAssemblies $references -TypeDefinition @'
using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Runtime.InteropServices;
public sealed class StillroomPixelReport {
    public bool SameSize;
    public int Width;
    public int Height;
    public long ChangedPixels;
    public double MeanRgbDelta;
    public int MaxRgbDelta;
    public long ChangedAlpha;
}
public static class StillroomCaptureDifference {
    public static StillroomPixelReport Compare(string a,string b) {
        using(var before=new Bitmap(a)) using(var after=new Bitmap(b)) {
            var result=new StillroomPixelReport{Width=before.Width,Height=before.Height,SameSize=before.Width==after.Width&&before.Height==after.Height};
            if(!result.SameSize)return result;
            var rectangle=new Rectangle(0,0,before.Width,before.Height);
            var x=before.LockBits(rectangle,ImageLockMode.ReadOnly,PixelFormat.Format32bppArgb);
            var y=after.LockBits(rectangle,ImageLockMode.ReadOnly,PixelFormat.Format32bppArgb);
            try {
                var xb=new byte[Math.Abs(x.Stride)*before.Height];
                var yb=new byte[Math.Abs(y.Stride)*after.Height];
                Marshal.Copy(x.Scan0,xb,0,xb.Length);Marshal.Copy(y.Scan0,yb,0,yb.Length);
                long delta=0;
                for(int row=0;row<before.Height;++row) {
                    int xr=x.Stride>=0?row*x.Stride:(before.Height-row-1)*-x.Stride;
                    int yr=y.Stride>=0?row*y.Stride:(after.Height-row-1)*-y.Stride;
                    for(int column=0;column<before.Width;++column) {
                        int px=xr+column*4,py=yr+column*4;bool changed=false;
                        for(int channel=0;channel<3;++channel) {
                            int difference=Math.Abs(xb[px+channel]-yb[py+channel]);
                            delta+=difference;result.MaxRgbDelta=Math.Max(result.MaxRgbDelta,difference);
                            changed|=difference!=0;
                        }
                        if(xb[px+3]!=yb[py+3]){++result.ChangedAlpha;changed=true;}
                        if(changed)++result.ChangedPixels;
                    }
                }
                result.MeanRgbDelta=(double)delta/(before.Width*before.Height*3L);
            } finally {before.UnlockBits(x);after.UnlockBits(y);}
            return result;
        }
    }
}
'@
}
$baselineRoot = (Resolve-Path -LiteralPath $BaselineDirectory).Path
$actualRoot = (Resolve-Path -LiteralPath $ActualDirectory).Path
$rows = @()
foreach ($mode in $Modes) {
    $modeRoot = Join-Path $baselineRoot $mode
    foreach ($file in Get-ChildItem -LiteralPath $modeRoot -Filter '*.png') {
        $relativePath = $mode + '/' + $file.Name
        $actualPath = Join-Path $actualRoot $relativePath
        if (-not (Test-Path -LiteralPath $actualPath)) { throw "Missing capture: $relativePath" }
        $pixels = [StillroomCaptureDifference]::Compare($file.FullName,$actualPath)
        $rows += [ordered]@{file=$relativePath;sameSize=$pixels.SameSize;width=$pixels.Width;height=$pixels.Height;changedPixels=$pixels.ChangedPixels;meanRgbDelta=$pixels.MeanRgbDelta;maxRgbDelta=$pixels.MaxRgbDelta;changedAlpha=$pixels.ChangedAlpha;automaticQuality=($file.Name -like '*Automatic*')}
    }
}
$result = [ordered]@{captures=$rows.Count;exact=@($rows|Where-Object {$_.sameSize -and $_.changedPixels -eq 0}).Count;sizeMismatch=@($rows|Where-Object {-not $_.sameSize}).Count;scope='Actual PNG pixel readback. Automatic quality may select different tiers. Numerical differences require visual judgment; this tool does not score beauty or grant visual acceptance.';results=$rows}
$result | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath $OutputFile -Encoding UTF8
[ordered]@{captures=$result.captures;exact=$result.exact;sizeMismatch=$result.sizeMismatch;report=$OutputFile} | ConvertTo-Json -Compress
