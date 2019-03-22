<template>
  <div>
    <HexViewerLine
      v-for="(chunk, index) in byteChunks"
      :address="bytesPerLine * index"
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

export default {
  name: "HexEditor",
  components: { HexViewerLine },
  props: {
    startAddress: {
      type: Number,
      default: 0
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
      return chunk(this.bytes, this.bytesPerLine);
    }
  },
  methods: {
    updateByte: function(event) {
      const { address, byte } = event;
      const index = address - this.startAddress;
      this.bytes.splice(index, 1, byte);
      console.log(this.bytes);
    }
  }
};
</script>

<style>
</style>
