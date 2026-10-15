# A quiet interface with somewhere to look

Stillroom is a listening room, not a feed. Its interface succeeds when a person
can choose a place to rest, recognize what is selected, enter it immediately,
and return to their work without an extra decision. The landscape may be rich;
the operations should be orderly.

For this release, we define beauty as **a coherent place one wants to stay in,
whose purpose and next action are clear**. This is a design judgment for this
application, not a universal formula or evidence of improved attention.

## What the research changed

Visual simplicity is only one part of aesthetics. Moshagen and Thielsch's
VisAWI research distinguishes simplicity, diversity, colorfulness and craftsmanship.
We use those as complementary lenses: reduce competing controls, retain variety
in the eight landscapes, constrain the interface palette, and make alignment and
state feedback consistent. Their instrument was developed for websites; this
native app has not been validated with that scale.
[Original study](https://www.uni-muenster.de/OWMS/uploads/drafts_thielsch/pdf/moshagen_2010.pdf).

Calm technology describes movement between the center and periphery of attention.
Here the same screen changes its role: static accompaniment while working,
chosen scenery during a pause, and controls only when needed. No automatic
scene switching, timer, notification or animated interface is added.
[Weiser and Brown](https://calmtech.com/papers/designing-calm-technology).

Progressive disclosure works only if the split is understandable and the next
level is discoverable. Three flat, named pages separate Rest, Background and
Music & app. Nothing nests inside another settings menu. The rest page opens
first; the last visited page remains available during the session.
[NN/g on progressive disclosure](https://www.nngroup.com/articles/progressive-disclosure/).

Minimalism does not mean removing signs of interaction. Buttons keep labels,
scene choices keep names, selected values have an outline or underline as well
as a color change, and keyboard focus has a visible ring. The primary action
has the strongest color; secondary actions remain legible.
[NN/g on aesthetic and minimalist design](https://www.nngroup.com/articles/aesthetic-minimalist-design/).

Typography, proximity and consistent relationships give a screen hierarchy.
A large selected scene sits beside a stable catalogue; motion and brightness
sit directly below it. A single quiet surface contains the controls, without
blur, decorative motion or multiple floating panels.
[Microsoft design principles](https://learn.microsoft.com/en-us/windows/apps/design/design-principles),
[typography](https://learn.microsoft.com/en-us/windows/apps/design/signature-experiences/typography),
and [Rams's account of the whole product](https://www.vitsoe.com/eu/voice/design-by-vitsoe).

## Concrete choices

- A 98-pixel player panel replaces the previous 160-pixel panel. Playback, volume,
  a pause in the scenery, and settings remain immediately available. Quit lives
  in Music & app and the normal window close control.
- The selected landscape has the largest preview. All eight alternatives remain
  visible together; the catalogue never scrolls or changes order automatically.
- Motion and brightness are explicit choices. There is no need to cycle a button
  repeatedly to discover the current or available setting.
- Working backgrounds have their own preview. Plain colors, the illustrated lake
  and the image library are named choices; picture import and removal stay here.
- Drawing, pointer targets and keyboard order consume one pure `Interface`
  description. Tab and Shift-Tab traverse actions; Enter or Space activates the
  focused action. Without control focus, Space retains playback behavior.
- Rest still accepts only exit. Tab, Space and settings shortcuts cannot expose
  controls or change music while the scene is being viewed.

A larger dashboard would compete with the landscape. An unlabeled icon-only
screen would hide meaning. The chosen arrangement keeps descriptive choices
where they help, and removes chrome that does not help a decision.

## What was checked, and what remains a judgment

Property checks cover all pages and both languages over supported window sizes:
visible targets are inside the window, at least 24 pixels in each dimension,
do not overlap, and resolve to their displayed action. Keyboard traversal reaches
every action and wraps in both directions. The renderer is captured at 700×500
in English and Japanese, plus a larger gallery, to inspect clipping and spacing.
These checks do not measure task completion time or preference.

The palette uses warm light text on a dark green surface, with a pale green
primary action. Normal text roles exceed 4.5:1 contrast on their control surfaces;
focus uses a two-pixel light outline. W3C contrast and target guidance provide
useful checks, not a claim that a custom-drawn Windows app is WCAG certified.
It still lacks a complete screen-reader control tree.
[Text contrast](https://www.w3.org/WAI/WCAG20/Understanding/contrast-minimum.html),
[target size](https://www.w3.org/WAI/WCAG22/Understanding/target-size-minimum),
[focus appearance](https://www.w3.org/WAI/WCAG22/Understanding/focus-appearance).

Aesthetics and perceived usability can influence each other, but beauty does not
prove that a task became easier. Early experimental work found a relationship;
later work also showed that usability problems can reduce aesthetic judgments.
[Tractinsky et al.](https://cris.bgu.ac.il/en/publications/what-is-beautiful-is-usable-2/),
[Tuch et al.](https://edoc.unibas.ch/entities/publication/ebcfa3de-1a92-4195-b6fd-ab67d80af2aa).
The observable results here are a smaller player, direct choices, consistent
keyboard access, legible captured screens and lower rendering overhead. Whether
this feels beautiful and helps a particular person return to work remains their
experience to judge.
