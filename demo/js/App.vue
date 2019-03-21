<template>
  <div id="app">
    <HexEditor></HexEditor>
  </div>
</template>

<script>
import HexEditor from "./components/HexEditor.vue";

export default {
  name: "app",
  components: {
    HexEditor
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
