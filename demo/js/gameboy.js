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
    console.log('exec frame');
    const imageData = new Uint8ClampedArray(SCREEN_SIZE * 4);
    for (let i = 0; i < SCREEN_SIZE; i++) {
      let color = this.colors[screen[i]];
      for (let k = 0; k < 4; k++) {
        imageData[i + k] = color[k];
      }
    }
    this.context.putImageData(new ImageData(imageData, SCREEN_WIDTH, SCREEN_HEIGHT), 0, 0);
  }

  step() {
    if (this.core) {
      this.core.dbg_step(this.screen);
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

  start() {
    if (this.core && !this.interval) {
      this.core.run();
      this.interval = setInterval(() => this.renderFrame(), 1);
      // for (let i = 0; i < 1000; i++) {
      //   console.log(i);
      //   this.renderFrame();
      // }
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
