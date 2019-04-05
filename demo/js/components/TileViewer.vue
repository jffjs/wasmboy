<template>
  <div>
    <canvas ref="tileCanvas" width="160" height="160"></canvas>
  </div>
</template>

<script>
export default {
  name: "TileViewer",
  props: ["tileImage"],
  watch: {
    tileImage: function(tileImage) {
      this.ctx.putImageData(tileImage, 0, 0);
    }
  },
  mounted: function() {
    this.ctx = this.$refs.tileCanvas.getContext("2d");
  }
};
</script>

<style scoped>
canvas {
  border: solid 1px black;
}
</style>
