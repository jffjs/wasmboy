<template>
  <div id="app">
    <HexViewer :editable="true" :startAddress="0" :bytes="rom"></HexViewer>
  </div>
</template>

<script>
import range from "lodash-es/range";
import HexViewer from "./components/HexViewer.vue";

export default {
  name: "app",
  components: {
    HexViewer
  },
  data: function() {
    const rom = range(0, 256);
    return { rom: rom };
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
</style>
