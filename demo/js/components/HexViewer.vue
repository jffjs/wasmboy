<template>
  <div>
    <HexViewerLine
      v-for="(chunk, index) in byteChunks"
      :address="startAddress + bytesPerLine * index"
      :pc="pc"
      :bytes="chunk"
      :key="index"
      :editable="editable"
      @update-byte="updateByte"
    ></HexViewerLine>
  </div>
</template>

<script>
import HexViewerLine from "./HexViewerLine.vue";
import chunk from "lodash-es/chunk";
import take from "lodash-es/take";

export default {
  name: "HexEditor",
  components: { HexViewerLine },
  props: {
    pc: Number,
    startAddress: {
      type: Number,
      default: 0
    },
    lines: {
      type: Number,
      default: 12
    },
    bytes: Array,
    bytesPerLine: {
      type: Number,
      default: 8
    },
    editable: {
      type: Boolean,
      default: false
    }
  },
  computed: {
    byteChunks: function() {
      return take(
        chunk(this.bytes.slice(this.startAddress), this.bytesPerLine),
        this.lines
      );
    }
  },
  methods: {
    updateByte: function(event) {
      const { address, byte } = event;
      const index = address - this.startAddress;
      this.bytes.splice(index, 1, byte);
    }
  }
};
</script>

<style>
</style>
