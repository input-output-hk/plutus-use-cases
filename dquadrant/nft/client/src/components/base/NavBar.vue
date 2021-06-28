<template>
  <b-navbar toggleable="lg" style="border-bottom: 1px solid #E8E8E8">
    <b-navbar-brand>
      <img src="public/favicon.ico" alt="Logo" height="30px">
    </b-navbar-brand>
    <b-navbar-nav>
      <b-nav-item :active="$router.currentRoute.path.startsWith('/market')" to="/market">Market</b-nav-item>
      <b-nav-item :active="$router.currentRoute.path.startsWith('/dummy-auction')" to="/dummy-auction">Auction</b-nav-item>
      <b-nav-item :active="$router.currentRoute.path.startsWith('/mint')" to="/mint">Mint</b-nav-item>
      <b-nav-item v-b-toggle.sidebar-1>Wallet</b-nav-item>
      <b-sidebar id="sidebar-1" title="Wallet" shadow>
        <SideBar/>
      </b-sidebar>
    </b-navbar-nav>
    <b-navbar-nav class="ml-auto">
      <slot></slot>
      <b-form-select v-on:change="updateIndex" v-model="instanceIndex" :options="options"></b-form-select>
      <span style="min-width: 250pt" class="p-2  text-muted text-sm-left">{{ instanceId }}</span>
    </b-navbar-nav>
  </b-navbar>
</template>

<script>
import SideBar from "./SideBar";

export default {
  name: "NavBar",
  components: {SideBar},
  computed: {
    instanceId() {
      return this.$store.state.contract.instance.cicContract.unContractInstanceId
    },
    options() {
      return this.$store.state.contract.instances.map((x, i) => {
        return {text: 'Wallet-' + x.cicWallet.getWallet, value: i}
      })
    },
    instanceIndex: {
      get: function () {
        return this.$store.state.contract.activeIndex
      },
      set: function (x){
        console.log("Set Wallet - "+x)
      }
    }
  },
  watch: {
    instanceIndex(x) {
      this.$store.state.contract.instance = this.$store.state.contract.instances[x]
      this.$http.post(
        `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/funds`,"\"\""
      )
    }
  },
  methods: {
    updateIndex(e) {
      this.$store.state.contract.activeIndex=e
      this.$store.state.contract.instance=this.$store.state.contract.instances[e]
    },
  },
  data: function () {
    return {}
  }
}
</script>

<style scoped>

</style>
