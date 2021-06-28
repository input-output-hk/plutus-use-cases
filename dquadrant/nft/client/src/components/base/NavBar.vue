<template>
  <b-navbar toggleable="md" class="shadow-sm pl-5">
    <b-navbar-brand class="navbar-brand">
      <img src="https://placekitten.com/g/30/30" alt="Logo" height="30px">
    </b-navbar-brand>
    <b-navbar-toggle target="nav-collapse"></b-navbar-toggle>
    <b-collapse id="nav-collapse" is-nav>
      <b-navbar-nav>
        <b-nav-item :active="$router.currentRoute.path.startsWith('/market')" to="/market">Market</b-nav-item>
        <b-nav-item :active="$router.currentRoute.path.startsWith('/mint')" to="/mint">Mint</b-nav-item>
        <b-nav-item :active="$router.currentRoute.path.startsWith('/auction')" to="/auction">Auction</b-nav-item>
        <b-nav-item v-b-toggle.main-sidebar>Wallet</b-nav-item>
        <SideBar/>
      </b-navbar-nav>
      <b-navbar-nav class="ml-auto">
        <slot></slot>
        <b-form-select v-on:change="updateIndex" v-model="instanceIndex" :options="options"></b-form-select>
        <b-nav-item class="p-2 text-muted text-sm-left">{{ instanceId }}</b-nav-item>
      </b-navbar-nav>
    </b-collapse>
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
      set: function (x) {
        console.log("Set Wallet - " + x)
      }
    }
  },
  watch: {
    instanceIndex(x) {
      this.$store.state.contract.instance = this.$store.state.contract.instances[x]
      this.$http.post(
          `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/funds`, "\"\""
      )
    }
  },
  methods: {
    updateIndex(e) {
      this.$store.state.contract.activeIndex = e
      this.$store.state.contract.instance = this.$store.state.contract.instances[e]
    },
  },
  data: function () {
    return {}
  }
}
</script>

<style scoped>

</style>
