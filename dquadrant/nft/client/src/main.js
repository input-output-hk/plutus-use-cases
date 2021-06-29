import Vue from "vue";
import VueRouter from "vue-router";

import "./config/bootstrap-vue";
import "./config/vue-toastr";
import "./config/axios-vue";
import "./config/progress-vue";
import { store } from "./config/vuex";

import "./assets/main.css";

import Base from "@/components/base/Base";
import Market from "./components/Market";
import MIntToken from "./components/MIntToken";
import DummyAuction from "./components/DummyAuction";

const routes = [
  {
    path: "",
    redirect: "market"
  },
  {
    path: "/market",
    name: "market",
    component: Market
  },
  {
    path: "/mint",
    name: "mint",
    component: MIntToken
  },
  {
    path: '/dummy-auction',
    name: 'auction',
    component: DummyAuction
  }
];

const router = new VueRouter({
  routes,
  mode: "history"
});
Vue.use(VueRouter);

Vue.config.productionTip = false;

new Vue({
  router,
  store,
  render: h => h(Base)
}).$mount("#app");