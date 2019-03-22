<template>
  <div>
    <HexEditorLine
      v-for="(chunk, index) in byteChunks"
      :address="bytesPerLine * index"
      :bytes="chunk"
      :key="index"
      :editable="editable"
      @update-byte="updateByte"
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
