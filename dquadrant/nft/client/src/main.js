import Vue from "vue";
import VueRouter from "vue-router";

import "./config/bootstrap-vue";
import "./config/vue-toastr";
import "./config/axios-vue";
import "./config/progress-vue";
import { store } from "./config/vuex";

import "./assets/main.css";

import App from "@/App";

import Market from "./components/Market";
import MIntToken from "./components/MIntToken";

import Auction from "@/pages/Auction";

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
    path: "/auction",
    name: "Auction",
    component: Auction
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
  render: h => h(App)
}).$mount("#app");
