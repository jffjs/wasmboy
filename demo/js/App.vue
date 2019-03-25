<template>
  <div id="app">
    <div class="debugger">
      <HexViewer :editable="true" :pc="cpuSnapshot.pc" :bytes="rom"></HexViewer>
      <CpuSnapshot id="cpu-snapshot" :snapshot="cpuSnapshot"></CpuSnapshot>
      <button @click="step()">Step</button>
      <button @click="reset()">Reset</button>
    </div>
  </div>
</template>

<script>
import Vue from "vue";
import range from "lodash-es/range";
import CpuSnapshot from "./components/CpuSnapshot.vue";
import HexViewer from "./components/HexViewer.vue";
import { Gameboy } from "../../pkg";

const rom = new Uint8Array(1000);
rom[0x100] = 0x04;
rom[0x101] = 0x04;
rom[0x102] = 0x3d;

let gb = new Gameboy(rom, true);

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
    step: function() {
      gb.dbg_step();
      this.cpuSnapshot = gb.dbg_cpu_snapshot();
    },
    reset: function() {
      gb = new Gameboy(Uint8Array.from(this.rom), true);
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
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}

.debugger {
  display: flex;
}
#cpu-snapshot {
  width: 260px;
}
</style>
