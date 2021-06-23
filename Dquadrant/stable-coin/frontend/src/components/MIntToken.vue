<template>
  <div>
    <NavBar>
    </NavBar>
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
  methods: {
    onsubmit() {
      this.$task.start()
      this.$http.post(
        `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/mint`
        , this.$refs.my_input.value
      ).then(() => {
        this.$task.infoMessage("Transaction Submitted. Wait for it to get confirmed")

        }
      )
    }
  },
  data: () => {
    return {
      token_name: "",
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
