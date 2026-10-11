# Using Stillroom

Extract the ZIP to a permanent folder and open `stillroom.exe`. The four built-in
presets play automatically in shuffled order. Create a shortcut to the executable
or pin it to Start for a direct launch. Launching again brings the existing window
forward instead of starting a second player.

On first launch, a second monitor is preferred when available. The default screen
is static dark green. Move the mouse over it to reveal controls. In fullscreen,
controls disappear after 4.5 seconds without activity. The Display menu stays
visible until closed. Compact mode always keeps controls visible.

Fullscreen uses a borderless desktop window and does not request a resolution
or refresh-rate change. Its visible area is confined to the selected display.

| Control | Action |
|---|---|
| Space | Pause or resume |
| N | Next track |
| + / - | Adjust volume |
| Click volume bar | Set volume |
| S / Display | Open or close settings |
| F11 | Fullscreen or compact window |
| Esc | Close settings, or return to compact mode |
| M | Move to the next monitor |
| O | Choose a music folder |
| P | Return to built-in music |
| I | Add a background image |
| [ / ] | Previous / next background image |
| B | Cycle image brightness: 100%, 75%, 50% |
| x / Alt+F4 | Exit |

Choose **Music folder** to use MP3, WAV or OGG files. The folder is read directly,
and the picker opens at your current or last selected folder. Switching to
built-in music remembers that location for next time. You can also enter a
folder path in the picker's text field. Music is loaded
without copying or changing files. Subfolders are not scanned. The limits are
1,000 candidate files and 128 MiB per file. Corrupt recordings are skipped; an
unavailable library falls back to built-in music at startup. Removing files during
playback can affect future loads. FLAC, playlists, streaming and per-track
loudness normalization are not supported in this release.

Linen, Moss, Orbit and Night are three-minute algorithmic scores with soft tones
and a restrained pulse. They are generated in small buffers, not downloaded or
stored as recordings. Choose a comfortable volume for your listening environment.

In **Display**, choose **Show image** to see the image library. Four static
wallpapers are included: Mist lake, Quiet forest, Soft dunes and Night field.
Use **Previous / Next** to choose one. Images keep their aspect ratio and fill
the window by cropping the edges; there is no automatic slideshow or animation.

Use **Add image** for your own PNG, JPEG or BMP. Images are copied into
`%APPDATA%/Stillroom/backgrounds/`, so moving the original does not break the
registration. **Remove imported image** removes the currently displayed managed
copy; it never deletes your original or bundled backgrounds. The decoder accepts
up to 32 MiB, 8192 pixels per edge and 32 megapixels, then limits the display copy
to 3840 pixels on its longest edge. Animated formats are not offered.

**Image brightness** cycles between 100%, 75% and 50%. Image, brightness and
music choices are independent. The included images use restrained palettes and
mostly broad shapes; choose the one you find least distracting, or use a plain
color. These are design choices, not tested ADHD treatment effects.

Settings are saved in `%APPDATA%/Stillroom/settings.json`: volume, language,
background, selected image, brightness, window mode, monitor and music folder. Close the app and delete that
file to reset. It is not part of the release and is never transmitted.

While playing, Stillroom asks Windows to keep the system awake; fullscreen also
keeps the display awake. Pause or exit releases these requests. Compact mode
releases the display request. Other apps may make their own sleep requests.

For paper reading, leave the quiet screen on a nearby display. For PDFs or code,
place it on a spare display. These are workspace options, not promises of improved
attention. The app does not block notifications or control other applications.
