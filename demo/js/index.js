import Vue from 'vue';
import App from './App.vue';

Vue.config.productionTip = false;

new Vue({
  render: h => h(App)
}).$mount('#app');

import { Gameboy } from '../../pkg';

const rom = new Uint8Array(1000);
rom[0x100] = 0x04;
rom[0x101] = 0x04;
rom[0x102] = 0x3d;

const gb = new Gameboy(rom, true);
gb.dbg_step();
console.log(gb.dbg_cpu_snapshot());
gb.dbg_step();
console.log(gb.dbg_cpu_snapshot());
gb.dbg_step();
console.log(gb.dbg_cpu_snapshot());
