<template>
  <b-navbar toggleable="lg" style="border-bottom: 1px solid #E8E8E8">
    <b-navbar-brand>
      <img src="logo.png" alt="Logo" height="30px">
    </b-navbar-brand>
    <b-navbar-nav>
      <b-nav-item :active="$router.currentRoute.path.startsWith('/dashboard')" to="/dashboard">Dashboard</b-nav-item>
      <b-nav-item :active="$router.currentRoute.path.startsWith('/oracle')" to="/oracle">Oracle</b-nav-item>
      <b-nav-item :active="$router.currentRoute.path.startsWith('/configure')" to="/configure">Configure Contract</b-nav-item>
      <b-nav-item :active="$router.currentRoute.path.startsWith('/mint')" to="/mint">Mint / Redeem</b-nav-item>
    </b-navbar-nav>
    <b-navbar-nav class="ml-auto">
      <slot></slot>
      <b-form-select v-if="!showOracleId" v-on:change="updateIndex" v-model="instanceIndex" :options="optionsWithNoOracle"></b-form-select>
    </b-navbar-nav>
  </b-navbar>
</template>

<script>

export default {
  name: "NavBar",
  mounted() {
    if(!this.showOracleId){
      this.postStatusCall()
    }
  },
  computed: {
    showOracleId(){
      return this.$router.currentRoute.path.includes("oracle") ||
             this.$router.currentRoute.path.includes("configure")
    },
    oracleId(){
      const index =  this.$store.state.contract.instances.map((x, i) => {
        return {text: 'Wallet-' + x.cicWallet.getWallet + " " + x.cicDefinition.tag, value: i}
      })?.filter(item => item.text.includes("Oracle"))?.[0]["value"]
      return this.$store.state.contract.instances[index]["cicContract"]["unContractInstanceId"]
    },
    instanceId() {
      return this.$store.state.contract.instance.cicContract.unContractInstanceId
    },
    instanceDefinition() {
      return this.$store.state.contract.instance.cicDefinition.tag
    },
    allOptions(){
      return this.$store.state.contract.instances.map((x) => {
        return {text: 'Wallet-' + x.cicWallet.getWallet + " " + x.cicDefinition.tag, value: x.cicContract.unContractInstanceId}
      })
    },
    optionsWithNoOracle() {
      return this.$store.state.contract.instances.map((x, i) => {
        return {text: 'Wallet-' + x.cicWallet.getWallet + " " + x.cicDefinition.tag, value: i}
      }).filter(item => !item.text.includes("Oracle"))
    },
    instanceIndex: {
      get: function () {
        return this.$store.state.contract.activeIndex
      },
      set: function (x){
        console.log(x)
      }
    }
  },
  watch: {
    instanceIndex(x) {
      this.$store.state.contract.instance = this.$store.state.contract.instances[x]
      this.postStatusCall()
    }
  },
  methods: {
    updateIndex(e) {
      this.$store.state.contract.activeIndex=e
      this.$store.state.contract.instance=this.$store.state.contract.instances[e]
    },
    postStatusCall(){
      this.$http.post(
          `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/funds`,"\"\""
      ).finally(() => this.$http.post(
          `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/currentRates`,"\"\""
      )).finally(() => this.$http.post(
          `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/currentState`,"\"\""
      ))
    }
  },
  data: function () {
    return {}
  }
}
</script>
<style scoped>
.custom-select{
  min-width: fit-content;
}
</style>
