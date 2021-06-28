<template>
  <div>
    <NavBar/>
    <b-container>
      <b-row>
        <b-col>
          <b-form @submit.prevent="onsubmit()">
            <b-form-group label="Token name:" label-for="token-name">
              <b-form-input id="token-name" ref="my_input" type="text" placeholder="Enter token name" required>
              </b-form-input>
            </b-form-group>
            <b-button type="submit" variant="primary">Mint</b-button>
          </b-form>
        </b-col>
      </b-row>
    </b-container>

  </div>
</template>

<script>
import NavBar from "./base/NavBar";

export default {
  name: "MIntToken",
  components: {NavBar},
  created() {
    if(this.instanceId){
      this.refresh()
    }
    this.items=[]
  },
  methods: {
    onsubmit() {
      this.shouldTrigger = true
      this.$task.do(
          this.$http.post(
              `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/mint`
              , JSON.stringify(this.$refs.my_input.$el.value)
          ).then(() => {
                this.$task.infoMessage("Transaction Submitted. After Transaction is confirmed, you will receive the NFT in your wallet")
              }
          )
      )
    }
  },
  computed: {
    lastResponse() {
      return this.$store.state.contract.lastObservable
    }
  },
  data: () => {
    return {
      token_name: "",
      shouldTrigger: false
    }
  },
  watch: {
    lastResponse() {
      if (this.shouldTrigger) {
        this.shouldTrigger = false
        this.$task.successMessage(this.token_name + " Minted Successfully")
      }
    }
  }
}
</script>

<style scoped>
.body {
  margin: 0;
  background: #eee;
}
</style>
