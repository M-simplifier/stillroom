# Using Stillroom

Extract the ZIP to a permanent folder and open `stillroom.exe`. The four built-in
presets play automatically in shuffled order. Create a shortcut to the executable
or pin it to Start for a direct launch. Launching again brings the existing window
forward instead of starting a second player.

On first launch, a second monitor is preferred when available. The default screen
is static dark green. Move the mouse over it to reveal controls. In fullscreen,
controls disappear after 4.5 seconds without activity. The Settings menu stays
visible until closed. Compact mode always keeps controls visible.

Fullscreen uses a borderless desktop window and does not request a resolution
or refresh-rate change. Its visible area is confined to the selected display.

| Control | Action |
|---|---|
| Tab / Shift-Tab | Focus the next / previous visible action |
| Enter / Space with a focused control | Activate that action |
| Space without control focus | Pause or resume |
| N | Next track |
| + / - | Adjust volume |
| Click volume bar | Set volume |
| R / click background while working | Enter a break; R always returns |
| S / Settings | Open or close settings |
| F11 | Fullscreen or compact window |
| Esc | Leave rest first; otherwise close settings or return to compact mode |
| M | Move to the next monitor |
| O | Choose a music folder |
| P | Return to built-in music |
| I | Add a background image |
| [ / ] | Previous / next background image |
| B (while working) | Cycle brightness; in the rest gallery, change rest brightness |
| A (in rest gallery) | Freeze / resume the preview |
| Music & app → Quit Stillroom / Alt+F4 | Exit |

In **Settings → Music & app**, choose **Choose a music folder** to use MP3, WAV or OGG files. The folder is read directly,
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

In **Settings → Background**, choose **My images** to see the image library. Six static
wallpapers are included: Mist lake, Quiet forest, Soft dunes, Night field,
Afternoon lake and Woodland pond.
Use **Previous / Next** to choose one. Images keep their aspect ratio and fill
the window by cropping the edges; there is no automatic slideshow or animation.

Use **Add image** for your own PNG, JPEG or BMP. Images are copied into
`%APPDATA%/Stillroom/backgrounds/`, so moving the original does not break the
registration. **Remove imported copy** removes the currently displayed managed
copy; it never deletes your original or bundled backgrounds. The decoder accepts
up to 32 MiB, 8192 pixels per edge and 32 megapixels, then limits the display copy
to 3840 pixels on its longest edge. Animated formats are not offered.

**Image brightness** offers direct choices of 100%, 75% and 50%. Image, brightness and
music choices are independent. The included images use restrained palettes and
mostly broad shapes; choose the one you find least distracting, or use a plain
color. These are design choices, not tested ADHD treatment effects.

For the journey, choose **Settings → Rest → Journey**, select **The quiet line**,
then **Continue the journey**. A new world takes a few seconds to prepare while
music keeps playing. The railcar starts gently. Space stops/starts it; Up/Down
select one of three paces; E stops and steps onto nearby safe ground. Bridges
or steep banks can refuse a stop: press Space to continue to another place.

On foot, WASD walks and Space starts/stops automatic walking. Trees, large rocks
and water constrain the route. The parked car waits where you left it; E boards
when you return nearby, and Space departs. Bridge decks are vehicle-only; there
is no remote summon. Click toggles mouse look, Tab releases it, Left/Right turns
the view without a mouse, and F1 shows the controls. **Esc/R**, the visible return
button, or switching to another app returns to work immediately. Other music,
volume and settings controls stay isolated from the journey.

Your world seed and location are saved to `%APPDATA%/Stillroom/journey.json` when
you leave a break or close the app. The world freezes while you work or the app
is closed. **New landscape** replaces this bookmark with a new seed; there is no
journey history/undo. The existing music and display settings are unaffected.
See [journey design and verification](journeys.md).

For a quiet scene, click the background outside the controls, click **Take a pause**, or
press **R**. The scene appears immediately and the hint disappears after three
seconds. Choose the scene, motion and brightness beforehand under **Settings →
Rest** (the first page). The gallery has three **Nature** and eight **Light &
sky** scenes, with actual thumbnails and a live preview.
Click a thumbnail, set motion and brightness, then choose **Rest in this scene**.
These choices are saved immediately, including when closing the gallery.
The three flat tabs are Rest, Background, and Music & app. Use the close icon or
Esc to return to the player. Tab reveals controls when they are hidden; pointer
movement returns navigation to the mouse. The focused control has a light outline.
Rest uses its own scenery and brightness, preserving the working view.
Click, press **Esc/R**, or switch to another application to leave rest. The first
Esc restores the background without shrinking the window. Music keeps playing;
there is no timer, automatic finish, slideshow or notification. Rest never resumes
automatically when starting the app. During these eleven quiet scenes, all application actions except
exit are ignored, including Space, A/B, numbers, music, volume and window shortcuts.
The OS window-close action still works. See the [comparison guide](rest-mode.md).

Eight scenes are generated by a local shader: Water and light, Amber sea,
Sunlit shallows, Sea of clouds, Northern lights, Light veil, Starry night and
Flowing ink. They do not depend on image or video assets.
The procedural animation has a 240-second cycle. The gallery can freeze and resume
the preview; entering rest starts at time zero, so Still uses a repeatable frame.
If the shader cannot load, rest displays an explanation; click or Esc returns to work.
See the [shader research notes](shader-research.md) for techniques and references.

Settings are saved in `%APPDATA%/Stillroom/settings.json`: volume, language,
background, selected image, brightness, rest scene/motion/brightness, window mode,
monitor and music folder. Close the app and delete that
file to reset. It is not part of the release and is never transmitted.

While playing, Stillroom asks Windows to keep the system awake; fullscreen also
keeps the display awake. Pause or exit releases these requests. Compact mode
releases the display request. Other apps may make their own sleep requests.

For paper reading, leave the quiet screen on a nearby display. For PDFs or code,
place it on a spare display. These are workspace options, not promises of improved
attention. The app does not block notifications or control other applications.
