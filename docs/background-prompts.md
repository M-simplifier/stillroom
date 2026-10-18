# Background generation prompts

Created with the built-in OpenAI image generation tool: the first four on 2026-10-11, Afternoon lake and Woodland pond on 2026-10-15. Each image used one generation with no reference image. Final outputs are stored in `assets/backgrounds/`. The prompts below express design intent, not clinical evidence about ADHD or attention. Outputs were visually inspected before bundling. The app offers a fixed image, manual selection, dimming and plain colors.

## lake

Use case: stylized-concept. Create one finished wide 16:9 landscape wallpaper (2048x1152 or wider) for Stillroom, a quiet focus music player. Scene: an immense empty mist-covered lake in the blue-green hour before dawn, distant broad gently overlapping ridgelines dissolving into fog, smooth water and barely perceptible reflections. Refined atmospheric digital matte painting with restrained photographic naturalism, soft edges, broad tonal fields, muted deep sage and blue-grey, low local contrast, no white highlights. The calm center and bottom two thirds should have spacious almost uniform tonal variation. This is an unobtrusive static background, beautiful without demanding attention. No people, animals, boats, buildings, dramatic rays, sun disc, text, logo, border, stars, intricate ripples, dense detail, or animation. Full bleed landscape; not a mockup or collage.

## forest

Use case: stylized-concept. One finished 16:9 wide landscape wallpaper, 2048x1152 or wider, for an unobtrusive static focus music player. An extremely quiet misty woodland rendered as a refined painterly photographic landscape: a few large softly defined trunks around the edges, distant trees dissolving into deep green grey fog, smooth moss-colored ground. Broad shapes, no dense foliage or leaf microdetails. Muted sage, dark olive, and grey-green, low saturation and low local contrast. Soft diffuse overcast light, no bright shafts or sun. Open calm center and lower half, a spacious resting place for the eye. No people, wildlife, paths leading to mysterious destinations, objects, buildings, text, logos, frame, or collage. Full bleed, subtly beautiful, no dramatic centerpiece.

## dunes

Use case: stylized-concept. One finished wide 16:9 wallpaper, 2048x1152 or wider, for a quiet focus music player. Three very broad gently curved dunes under a smooth hazy overcast sky, contemplative minimalist landscape. Restrained soft matte painting with believable soft sand, nearly no grain or ripples. Muted warm taupe, dusty clay, grey beige, soft mauve shadows; mid-dark luminance and compressed contrast, not bright desert yellow. Horizon in upper third, long soft diagonal landforms with spacious low-detail center and foreground. No sun, sharp ridges, dramatic lighting, footprints, vegetation, animals, people, buildings, objects, text, logos, borders, or collage. Static, calm, understated visual warmth.

## night

Use case: stylized-concept. One finished wide 16:9 abstract wallpaper, 2048x1152 or wider, for a quiet focus music player. Sophisticated atmospheric color-field painting: broad layered clouds of deep indigo, charcoal blue and muted dusky violet, resembling a night sea and sky almost dissolving into each other, softly curved tonal transitions. Extremely subtle low contrast, matte, deep subdued midtones with no shiny or glowing elements. Spacious composition with a calm center and bottom half. No actual stars, moon, sun, aurora, neon, waves, particles, visible brush microtexture, fractals, objects, text, logos, frames or collage. Designed to remain still and gently recede from attention, yet have understated depth and color.

## Additional static landscapes

Afternoon lake and Woodland pond were originally generated for a rest-mode experiment. Their original prompts below mention later water animation; Stillroom 0.8.1 includes the unchanged PNGs only as optional, static working backgrounds. Rest scenes remain procedural. No runtime AI or network request is needed.

### Afternoon lake

Use case: photorealistic-natural. Asset type: final 16:9 full-screen background for a quiet desktop rest mode. Create one beautiful, believable, gently inviting view of a sheltered woodland lake in a mild early autumn afternoon, photographed at seated eye level. Full bleed 2560x1440 landscape or wider. The distant low shore, a few softly outlined deciduous trees in sage/olive green and slightly warm muted gold, and a softly clouded pale grey-blue sky occupy the TOP 42 percent. Below 46 percent of image height, the ENTIRE picture from left edge to right edge is unobstructed lake water, continuing to the bottom edge, with soft reflections and very delicate broad horizontal ripples. No foreground rocks, plants, branches, objects or shore anywhere in the lower 54 percent, because only this water region will later be very subtly animated by the app. A lightly shaded viewpoint and a warm, restful small opening in the distant trees give a place to rest the gaze without a strong focal object. Natural color and convincing depth, restrained photographic realism, quiet soft daylight, medium-dark values, gentle low-medium contrast, no dazzling white or sun glare. Enough water texture to quietly watch, no dense microdetail, no hard dramatic composition. Not foggy gloom, not fantasy, not an empty color gradient. No people, animals, boats, buildings, dock, bridges, text, logo, frame, sun disc, shafts of light or scattered bright sparkles. Deliver ONLY the finished landscape image, no mockup, no collage.

### Woodland pond

Use case: photorealistic-natural. Asset type: finished wide 16:9 desktop rest-mode landscape, full bleed, 2560x1440 or wider. A quiet Japanese-inspired woodland pond after light rain, from a low seated viewpoint. Photographic naturalism, subtly painterly restraint, believable textures and depth. Across the TOP 43 percent: a softly curved distant shoreline with mossy rounded stones at the water's edge, soft fern and a few slender maple trees, cool deep green reflected in a still pond, gentle pale overcast light filtering through an open area in the canopy. Below 47 percent: ENTIRE width consists only of unobstructed water with subtle reflections and soft very sparse ripple textures all the way down to the bottom; NO foreground stones, branches, reeds, islands or floating objects in the lower half. Keep the pond far bank above this line so the app can gently animate only the water later. Quiet light green opening slightly right of center in the upper part offers a place for the gaze to settle. A small intimate sheltered space, no mysterious dark tunnel, no path, no scenery to explore. Balanced medium-dark jade, grey green, muted moss, gentle warm grey rocks; softened highlights, modest contrast, enough material texture to feel real but no dense visual clutter or high frequency glitter. Beautiful without dramatic light. No humans, animals, koi, boats, buildings, temple, lanterns, bridges, fountains, waterfall, rain streaks, text, frame or logo. One finished landscape ONLY, no collage or UI mockup.

### Original output hashes

SHA-256 for the unchanged 2026-10-15 outputs:

- `05-afternoon-lake.png`: `cb0143d32a4bbc5f58d42952e7221033f08164ffad1fb21a72725a6f2da854dc`
- `06-woodland-pond.png`: `f1765b111879875433833d69c4bfa704a363088d7ccf9af957f4d92ecd8617bb`
