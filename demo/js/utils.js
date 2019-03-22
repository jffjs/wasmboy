export function toHex(val, pad) {
  return val.toString(16).padStart(pad, '0');
}
