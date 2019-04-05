import { Emulator, CPU } from '../../pkg';

const SCREEN_WIDTH = 160;
const SCREEN_HEIGHT = 144;
const SCREEN_SIZE = SCREEN_WIDTH * SCREEN_HEIGHT;

export class Gameboy {
  constructor(context2d, rom) {
    this.rom = rom;
    this.core = new Emulator(rom);
    this.context = context2d;
    this.screen = new Uint8Array(SCREEN_SIZE);
    this.colors = [[255, 255, 255, 255], [192, 192, 192, 255], [96, 96, 96, 255], [0, 0, 0, 255]];
  }

  loadROM(rom) {
    this.rom = rom;
    if (this.running) {
      this.pause();
    }
    this.core = new Emulator(rom);
  }

  renderFrame() {
    const screen = new Uint8Array(SCREEN_SIZE);
    this.core.frame(screen);
    const imageData = new Uint8ClampedArray(SCREEN_SIZE * 4);
    for (let i = 0; i < SCREEN_SIZE; i++) {
      let color = this.colors[screen[i]];
      for (let k = 0; k < 4; k++) {
        imageData[i * 4 + k] = color[k];
      }
    }
    this.context.putImageData(new ImageData(imageData, SCREEN_WIDTH, SCREEN_HEIGHT), 0, 0);
  }

  step() {
    if (this.core) {
      // this.core.dbg_step(this.screen);
      this.renderFrame();
    }
  }

  cpuSnapshot() {
    if (this.core) {
      return this.core.dbg_cpu_snapshot();
    }
  }

  memSnapshot(addr, length) {
    if (this.core) {
      return this.core.dbg_mem_snapshot(addr, length);
    }
  }

  // returns image data for a tile
  getTileImageData(tileNumber, imageSize = 160) {
    if (this.core) {
      const tileData = this.core.dbg_tile_data(tileNumber);
      const pixelSize = imageSize / 8;
      const imageData = new Uint8ClampedArray(imageSize * imageSize * 4);

      for (let i = 0; i < imageSize * imageSize; i++) {
        const py = Math.floor(i / imageSize / pixelSize);
        const px = Math.floor((i % imageSize) / pixelSize);
        const color = this.colors[tileData[py * 8 + px]];
        for (let k = 0; k < 4; k++) {
          imageData[i * 4 + k] = color[k];
        }
      }

      return new ImageData(imageData, imageSize, imageSize);
    }
  }

  start() {
    if (this.core && !this.interval) {
      this.core.run();
      this.interval = setInterval(() => this.renderFrame(), 1);
      this.running = true;
    }
  }

  pause() {
    if (this.running) {
      this.core.pause();
      clearInterval(this.interval);
    }
  }

  reset() {
    this.loadROM(this.rom);
  }
}
