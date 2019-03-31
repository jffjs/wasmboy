<template>
  <div id="app">
    <div class="debugger">
      <div class="memory-cpu" v-if="cpuSnapshot">
        <HexViewer :editable="true" :pc="cpuSnapshot.pc" :bytes="rom"></HexViewer>
        <CpuSnapshot id="cpu-snapshot" :snapshot="cpuSnapshot"></CpuSnapshot>
      </div>
      <div class="controls">
        <input type="file" @change="loadRom">
        <button @click="step()">Step</button>
        <button @click="reset()">Reset</button>
        <button @click="pause()">Pause</button>
        <button @click="start()">Start</button>
      </div>
    </div>
    <canvas ref="screen" id="screen" width="160" height="144"></canvas>
  </div>
</template>

<script>
import Vue from "vue";
import range from "lodash-es/range";
import CpuSnapshot from "./components/CpuSnapshot.vue";
import HexViewer from "./components/HexViewer.vue";
import { Gameboy } from "./gameboy";

let gb;
let rom = new Uint8Array(1000);
rom[0x100] = 0x04;
rom[0x101] = 0x04;
rom[0x102] = 0x3d;

const canvas = document.getElementById("screen");
export default {
  name: "app",
  components: {
    CpuSnapshot,
    HexViewer
  },
  data: function() {
    return {
      rom: Array.from(rom),
      cpuSnapshot: null
    };
  },
  mounted: function() {
    const canvas = this.$refs.screen;
    gb = new Gameboy(canvas.getContext("2d"), rom);
    this.cpuSnapshot = gb.snapshot();
  },
  methods: {
    loadRom: function(event) {
      const files = event.target.files;
      if (files && files.length > 0) {
        const reader = new FileReader();
        reader.onload = readerEvent => {
          rom = new Uint8Array(readerEvent.target.result);
          gb.loadROM(rom);
          this.rom = Array.from(rom);
        };
        reader.readAsArrayBuffer(files[0]);
      }
    },
    step: function() {
      gb.step();
      this.cpuSnapshot = gb.snapshot();
    },
    reset: function() {
      gb.reset();
      this.cpuSnapshot = gb.snapshot();
    },
    start: function() {
      gb.start();
    },
    pause: function() {
      gb.pause();
      this.cpuSnapshot = gb.snapshot();
    }
  }
};
</script>

<style>
#app {
  font-family: "Avenir", Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: #2c3e50;
  margin-top: 60px;
}

.memory-cpu {
  display: flex;
}
#cpu-snapshot {
  width: 260px;
}
</style>
