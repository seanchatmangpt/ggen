function clamp(num, min, max) {
  return Math.min(Math.max(num, min), max);
}

function inRange(num, min, max) {
  return num >= min && num <= max;
}

export { clamp, inRange };