# Using Stillroom

Extract the ZIP to a permanent folder and open `stillroom.exe`. The four built-in
presets play automatically in shuffled order. Create a shortcut to the executable
or pin it to Start for a direct launch. Launching again brings the existing window
forward instead of starting a second player.

On first launch, a second monitor is preferred when available. The default screen
is static dark green. Move the mouse over it to reveal controls. In fullscreen,
controls disappear after 4.5 seconds without activity. The Display menu stays
visible until closed. Compact mode always keeps controls visible.

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
| x / Alt+F4 | Exit |

Choose **Music folder** to use MP3, WAV or OGG files. The folder is read directly,
without copying or changing files. Subfolders are not scanned. The limits are
1,000 candidate files and 128 MiB per file. Corrupt recordings are skipped; an
unavailable library falls back to built-in music at startup. Removing files during
playback can affect future loads. FLAC, playlists, streaming and per-track
loudness normalization are not supported in this release.

Linen, Moss, Orbit and Night are three-minute algorithmic scores with soft tones
and a restrained pulse. They are generated in small buffers, not downloaded or
stored as recordings. Choose a comfortable volume for your listening environment.

Settings are saved in `%APPDATA%/Stillroom/settings.json`: volume, language,
background, window mode, monitor and music folder. Close the app and delete that
file to reset. It is not part of the release and is never transmitted.

While playing, Stillroom asks Windows to keep the system awake; fullscreen also
keeps the display awake. Pause or exit releases these requests. Compact mode
releases the display request. Other apps may make their own sleep requests.

For paper reading, leave the quiet screen on a nearby display. For PDFs or code,
place it on a spare display. These are workspace options, not promises of improved
attention. The app does not block notifications or control other applications.
