<template>
  <div class="cpu-snapshot">
    <div class="reg-group">
      <div class="reg-8">
        <span class="reg-label">A</span>
        {{snapshot.a | hex(2)}}
      </div>
      <div class="reg-8">
        <span class="reg-label">F</span>
        {{snapshot.f | hex(2)}}
      </div>
    </div>
    <div class="reg-group">
      <div class="reg-8">
        <span class="reg-label">B</span>
        {{snapshot.b | hex(2)}}
      </div>
      <div class="reg-8">
        <span class="reg-label">C</span>
        {{snapshot.c | hex(2)}}
      </div>
    </div>
    <div class="reg-group">
      <div class="reg-8">
        <span class="reg-label">D</span>
        {{snapshot.d | hex(2)}}
      </div>
      <div class="reg-8">
        <span class="reg-label">E</span>
        {{snapshot.e | hex(2)}}
      </div>
    </div>
    <div class="reg-group">
      <div class="reg-8">
        <span class="reg-label">H</span>
        {{snapshot.h | hex(2)}}
      </div>
      <div class="reg-8">
        <span class="reg-label">L</span>
        {{snapshot.l | hex(2)}}
      </div>
    </div>
    <div class="reg-16">
      <span class="reg-label">PC</span>
      {{snapshot.pc | hex(4)}}
    </div>
    <div class="reg-16">
      <span class="reg-label">SP</span>
      {{snapshot.sp | hex(4)}}
    </div>
  </div>
</template>

<script>
import { toHex } from "../utils";
export default {
  props: ["snapshot"],
  filters: {
    hex: toHex
  }
};
</script>

<style scoped>
.cpu-snapshot {
  text-align: center;
}
.reg-group {
  display: flex;
  align-items: center;
}
.reg-8,
.reg-16 {
  padding: 4px 0;
  border: solid 1px #000;
  margin: 2px;
}
.reg-8 {
  width: 50%;
}
</style>
