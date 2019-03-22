<template>
  <div class="line">
    <div class="address">{{address | hex(4)}}</div>
    <div
      class="byte"
      :contenteditable="editable"
      @input="updateByte($event, index)"
      @blur="padByte"
      v-for="(byte, index) in bytes"
      :key="index"
    >{{byte | hex(2)}}</div>
  </div>
</template>

<script>
import debounce from "lodash-es/debounce";

function toHex(val, pad) {
  return val.toString(16).padStart(pad, "0");
}

export default {
  name: "HexEditorLine",
  props: {
    address: Number,
    bytes: Array,
    editable: {
      type: Boolean,
      default: false
    }
  },
  methods: {
    updateByte: debounce(function(event, index) {
      const currentValue = toHex(this.bytes[index], 2);
      const newValue = event.target.innerText;
      const invalidHex = newValue ? !newValue.match(/^[\da-f]+$/i) : false;
      if (newValue.length > 2 || invalidHex) {
        event.target.innerText = currentValue;
        return;
      } else if (!newValue) {
        return;
      }
      const byte = parseInt(newValue, 16);
      if (byte != NaN) {
        const address = this.address + index;
        this.$emit("update-byte", { byte, address });
      }
    }, 250),
    padByte: function(event) {
      event.target.innerText = toHex(event.target.innerText, 2);
    }
  },
  filters: {
    hex: function(value, pad) {
      return toHex(value, pad);
    }
  }
};
</script>

<style>
.line {
  display: flex;
  font-family: "Courier New", Courier, monospace;
}

.address {
  border-right: solid 1px #000;
  padding-right: 4px;
  margin-right: 4px;
}

.byte {
  padding: 0 4px;
}
</style>
