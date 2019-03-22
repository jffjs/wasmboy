<template>
  <div id="app">
    <HexEditor :address="0" :bytes="rom"></HexEditor>
  </div>
</template>

<script>
import range from "lodash-es/range";
import HexEditor from "./components/HexEditor.vue";

export default {
  name: "app",
  components: {
    HexEditor
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
