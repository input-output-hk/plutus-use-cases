import Vue from 'vue'
import './config/bootstrap-vue'
import './config/vue-toastr'
import VueRouter from 'vue-router'
import {store} from './config/vuex'
import './config/axios-vue'
import './config/progress-vue'
import './assets/main.css'
// import '../src/util/piechartjs'
import Base from "./components/base/Base"
import UpdateFee from "./components/contract_owner/ContractOwnerPage"
import MintRedeemPage from "./components/mint_redeem/MintRedeemPage"
import Dashboard from "./components/dashboard/Dashboard"
import OraclePage from "./components/oracle/OraclePage"


const routes = [
  {
    path: '',
    redirect: 'dashboard'
  },
  {
    path: '/configure',
    name: 'configure',
    component: UpdateFee
  },
  {
    path: '/dashboard',
    name: 'dashboard',
    component: Dashboard
  },
  {
    path: '/mint',
    name: 'mint',
    component: MintRedeemPage
  },
  {
    path: '/oracle',
    name: 'oracle',
    component: OraclePage
  },
]

const router = new VueRouter({
  routes,
  mode: 'history'
})
Vue.use(VueRouter)

Vue.config.productionTip = false

new Vue({
  router,
  store,
  render: h => h(Base),
}).$mount('#app')
