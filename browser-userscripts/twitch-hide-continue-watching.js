// ==UserScript==
// @name     Hide "continue watching" section in Twitch
// @version  1
// @grant    none
// @match    https://www.twitch.tv/directory/following/videos
// ==/UserScript==

function hideCrap() {
  const cwSelector = '[aria-label="Continue Watching"]'
  const title = document.querySelector(cwSelector)
  if (!title) {
    setTimeout(hideCrap, 100)
    return
  }
  const div = document.querySelector(`${cwSelector} + div`)
  for (let curr of [title, div]) {
    curr.style.display = 'none'
  }
}

setTimeout(hideCrap, 500)