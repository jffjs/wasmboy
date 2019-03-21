<template>
  <div class="line">
    <div class="address">{{address | hex(4)}}</div>
    <div class="byte" v-for="(byte, index) in bytes" :key="index">{{byte | hex(2)}}</div>
  </div>
</template>

<script>
export default {
  name: "HexEditorLine",
  props: {
    address: Number,
    bytes: Array
  },
  filters: {
    hex: function(value, pad) {
      return value.toString(16).padStart(pad, "0");
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
