<template>
  <div>
    <HexEditorLine
      v-for="(chunk, index) in byteChunks"
      :address="bytesPerLine * index"
      :bytes="chunk"
      :key="index"
    ></HexEditorLine>
  </div>
</template>

<script>
import HexEditorLine from "./HexEditorLine.vue";
import chunk from "lodash-es/chunk";

export default {
  name: "HexEditor",
  components: { HexEditorLine },
  props: {
    address: Number,
    bytes: Array,
    bytesPerLine: {
      type: Number,
      default: 8
    }
  },
  computed: {
    byteChunks: function() {
      return chunk(this.bytes, this.bytesPerLine);
    }
  }
};
</script>

<style>
</style>
