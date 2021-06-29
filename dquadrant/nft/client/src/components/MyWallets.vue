<template>
  <div>
    <NavBar/>
    <b-row>
      <b-col cols="12" md="4" class="my-2" v-for="(option, i) in options" :key="i">
        <b-card :title="'Wallet '+ i" class="mx-2 shadow-sm">
          <b-card-text class="d-flex justify-content-between">
            <p></p>
            <b-button variant="primary" @click="updateIndex(i)" :disabled="instanceIndex === i">
              Select Wallet
            </b-button>
          </b-card-text>
        </b-card>
      </b-col>
    </b-row>
  </div>
</template>

<script>
import NavBar from "@/components/base/NavBar";

export default {
  name: "MyWallets",
  components: {NavBar},
  computed: {
    instanceId() {
      return this.$store.state.contract.instance.cicContract.unContractInstanceId
    },
    options() {
      return this.$store.state.contract.instances.map((x, i) => ({wallet: x.cicWallet.getWallet, value: i}))
    },
    instanceIndex: {
      get: function () {
        return this.$store.state.contract.activeIndex
      },
      set: function (x) {
        // this.$store.dispatch('updateContractInstance', this.$store.state.contract.instances[x])
        console.log(x)
      }
    }
  },
  watch: {
    instanceIndex() {
      // this.$store.dispatch('updateContractInstance', this.$store.state.contract.instances[x])
      this.$http.post(
          `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/funds`, "\"\""
      )
    }
  },
  methods: {
    updateIndex(e) {
      this.$store.dispatch('updateContractActiveIndex', e)
      this.$store.dispatch('updateContractInstance', this.$store.state.contract.instances[e])
    },
  },
}
</script>

<style scoped>

</style>
