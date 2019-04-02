<template>
  <div id="app">
    <div class="debugger">
      <div class="memory-cpu" v-if="cpuSnapshot">
        <div class="mem">
          <input placeholder="ff01" v-model="newAddr">
          <button @click="goToAddr">Go to</button>
          <HexViewer
            :editable="false"
            :pc="addr"
            :bytes="memSnapshot"
            :lines="hexViewerLines"
            :bytesPerLine="hexViewerBytesPerLine"
          ></HexViewer>
        </div>
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
export default {
  name: "app",
  components: {
    CpuSnapshot,
    HexViewer
  },
  data: function() {
    return {
      addr: 0,
      newAddr: null,
      memSnapshot: [],
      cpuSnapshot: null,
      hexViewerLines: 12,
      hexViewerBytesPerLine: 8
    };
  },
  mounted: function() {
    // gb = new Gameboy(canvas.getContext("2d"), rom);
    // this.updateSnapshots();
  },
  methods: {
    loadRom: function(event) {
      const canvas = this.$refs.screen;
      const files = event.target.files;
      if (files && files.length > 0) {
        const reader = new FileReader();
        reader.onload = readerEvent => {
          const rom = new Uint8Array(readerEvent.target.result);
          gb = new Gameboy(canvas.getContext("2d"), rom);
          this.updateSnapshots();
          this.addr = this.cpuSnapshot.pc;
        };
        reader.readAsArrayBuffer(files[0]);
      }
    },
    step: function() {
      gb.step();
      this.updateSnapshots();
    },
    reset: function() {
      gb.reset();
      this.updateSnapshots();
    },
    start: function() {
      gb.start();
    },
    pause: function() {
      gb.pause();
      this.updateSnapshots();
    },
    goToAddr: function() {
      this.addr = parseInt(this.newAddr, 16);
      this.updateSnapshots();
    },
    updateSnapshots: function() {
      this.cpuSnapshot = gb.cpuSnapshot();
      this.memSnapshot = Array.from(
        gb.memSnapshot(
          this.addr,
          this.hexViewerLines * this.hexViewerBytesPerLine
        )
      );
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
