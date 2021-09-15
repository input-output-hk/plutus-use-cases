<template>
  <div class="constraint-1080">
    <NavBar></NavBar>
    <div class="header h-centered-flex">
      <h1>Mint or Redeem Coins</h1>
      <span class="sub-header text-muted">One click action</span>
    </div>

    <p class="text-danger">
      {{ contractStatusText }}
    </p>

    <b-tabs content-class="mt-3">
      <b-tab title="Stable Coin" active>
        <StableMintRedeemPage/>
      </b-tab>
      <b-tab title="Reserve Coin">
        <ReserveMintRedeemPage/>
      </b-tab>
    </b-tabs>
  </div>

</template>

<script>
import ReserveMintRedeemPage from "./ReserveMintRedeemPage";
import StableMintRedeemPage from "./StableMintRedeemPage";
import NavBar from "../base/NavBar";
import {mapGetters} from "vuex";

export default {
  name: "MintRedeemPage",
  components: {
    NavBar,
    ReserveMintRedeemPage,
    StableMintRedeemPage,
  },
  // only mapState and mapGetters
  computed: {
    ...mapGetters({
      states: "getCurrentState"
    }),
    contractStatusText() {
      return this.states.contractStatus === "Paused" ?
          "*Note: Contract Paused : Cannot perform minting or redeeming of coins" : ""

    }
  },
}
</script>

<style scoped>
.header {
  margin: 48px 0;
  text-align: center;
}

.sub-header {
  font-size: 24px;
  font-style: normal;
  font-weight: normal;
  text-align: center;
}
</style>