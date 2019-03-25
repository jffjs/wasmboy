<template>
  <div id="app">
    <div class="debugger">
      <div class="memory-cpu">
        <HexViewer :editable="true" :pc="cpuSnapshot.pc" :bytes="rom"></HexViewer>
        <CpuSnapshot id="cpu-snapshot" :snapshot="cpuSnapshot"></CpuSnapshot>
      </div>
      <div class="controls">
        <input type="file" @change="loadRom">
        <button @click="step()">Step</button>
        <button @click="reset()">Reset</button>
      </div>
    </div>
  </div>
</template>

<script>
import Vue from "vue";
import range from "lodash-es/range";
import CpuSnapshot from "./components/CpuSnapshot.vue";
import HexViewer from "./components/HexViewer.vue";
import { Emulator } from "../../pkg";

let rom = new Uint8Array(1000);
rom[0x100] = 0x04;
rom[0x101] = 0x04;
rom[0x102] = 0x3d;

let gb = new Emulator(rom, true);

export default {
  name: "app",
  components: {
    CpuSnapshot,
    HexViewer
  },
  data: function() {
    return {
      rom: Array.from(rom),
      cpuSnapshot: gb.dbg_cpu_snapshot()
    };
  },
  methods: {
    loadRom: function(event) {
      const files = event.target.files;
      if (files && files.length > 0) {
        const reader = new FileReader();
        reader.onload = readerEvent => {
          rom = new Uint8Array(readerEvent.target.result);
          gb = new Emulator(rom, true);
          this.rom = Array.from(rom);
        };
        reader.readAsArrayBuffer(files[0]);
      }
    },
    step: function() {
      gb.dbg_step();
      this.cpuSnapshot = gb.dbg_cpu_snapshot();
    },
    reset: function() {
      gb = new Emulator(Uint8Array.from(this.rom), true);
      this.cpuSnapshot = gb.dbg_cpu_snapshot();
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
