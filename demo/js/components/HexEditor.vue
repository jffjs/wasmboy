<template>
  <div>
    <HexEditorLine :address="0" :bytes="[1, 2, 3, 4]"></HexEditorLine>
    <HexEditorLine :address="4" :bytes="[34, 52, 13, 44]"></HexEditorLine>
  </div>
</template>

<script>
import HexEditorLine from "./HexEditorLine.vue";
export default {
  name: "HexEditor",
  components: { HexEditorLine }
};
</script>

<style>
</style>
