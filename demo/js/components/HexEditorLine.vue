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
    updateByte: function(event, index) {
      const currentValue = toHex(this.bytes[index], 2);
      const newValue = event.target.innerText;
      if (newValue.length > 2) {
        event.target.innerText = currentValue;
        return;
      }
      const byte = parseInt(event.target.innerText, 16);
      if (byte != NaN) {
        this.bytes.splice(index, 1, byte & 0xff);
        console.log(this.bytes);
      }
    },
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
  padding-right: 2px;
  margin-right: 2px;
}

.byte {
  padding: 0 2px;
}
</style>
