"use strict";
const english = document.documentElement.lang === "en";
const copy = english ? {
  play: "Play", pause: "Pause", retry: "Retry playback",
  separate: "Combining individual boundaries retains two internal contact faces.",
  assembled: "The contact is internal, so no faces are generated there.",
  separateTitle: "Cross-section of two touching boxes. Taking each boundary first retains two internal contact faces.",
  assembledTitle: "Cross-section of two touching boxes. Extracting the boundary after assembly removes the internal faces."
} : {
  play: "再生する", pause: "一時停止", retry: "再生を再試行",
  separate: "各々の外面を足すと、接合部の2枚が残ります。",
  assembled: "接合部は内側になるので、面を生成しません。",
  separateTitle: "接する二つの箱の断面。各箱の外面を先に取ると中央の接合面2枚が残る。",
  assembledTitle: "接する二つの箱の断面。組み立ててから外面を取ると中央の接合面は消える。"
};
const before = document.querySelector("#before");
const after = document.querySelector("#after");
const play = document.querySelector("#play");
const time = document.querySelector("#time");
const clock = document.querySelector("#clock");
const split = document.querySelector("#split");
let running = false;
let tickId = 0;
function stop() {
  running = false;
  before.pause();
  after.pause();
  cancelAnimationFrame(tickId);
  play.textContent = copy.play;
}
function showTime(value) {
  time.value = value;
  clock.textContent = "0:" + Math.floor(value).toString().padStart(2, "0");
}
function tick() {
  if (!running) return;
  showTime(before.currentTime);
  if (Math.abs(before.currentTime - after.currentTime) > .06) after.currentTime = before.currentTime;
  tickId = requestAnimationFrame(tick);
}
play.addEventListener("click", async () => {
  if (running) { stop(); return; }
  if (before.currentTime >= 15.9) before.currentTime = 0;
  after.currentTime = before.currentTime;
  play.disabled = true;
  try {
    await Promise.all([before.play(), after.play()]);
    running = true;
    play.textContent = copy.pause;
    tick();
  } catch {
    stop();
    play.textContent = copy.retry;
  } finally { play.disabled = false; }
});
time.addEventListener("input", () => {
  stop();
  before.currentTime = Number(time.value);
  after.currentTime = Number(time.value);
  showTime(Number(time.value));
});
split.addEventListener("input", () => {
  document.querySelector("#afterLayer").style.clipPath = `inset(0 0 0 ${split.value}%)`;
  document.querySelector("#divider").style.left = split.value + "%";
});
before.addEventListener("ended", stop);
after.addEventListener("ended", stop);
document.addEventListener("visibilitychange", () => { if (document.hidden) stop(); });
const assembly = document.querySelector("#assembly");
const first = document.querySelector("#assembleFirst");
const separate = document.querySelector("#boundaryFirst");
function assemblyMode(splitFirst) {
  assembly.classList.toggle("separate", splitFirst);
  first.setAttribute("aria-pressed", String(!splitFirst));
  separate.setAttribute("aria-pressed", String(splitFirst));
  document.querySelector("#assemblyNote").textContent = splitFirst ? copy.separate : copy.assembled;
  document.querySelector("#assemblyCode").textContent = splitFirst
    ? "boundary partsA ++ boundary partsB"
    : "boundary (partsA ++ partsB)";
  document.querySelector("#assembly-title").textContent = splitFirst ? copy.separateTitle : copy.assembledTitle;
}
first.addEventListener("click", () => assemblyMode(false));
separate.addEventListener("click", () => assemblyMode(true));
