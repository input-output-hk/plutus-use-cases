<template>
  <div class="constraint-1080">
    <NavBar></NavBar>
    <div class="header h-centered-flex">
      <h1>Dashboard</h1>
      <span class="sub-header text-muted">One click action</span>
    </div>
    <b-row>
      <b-col lg="4" md="12" sm="12">
        <SigGenCard
          id="id-sig-usd"
          card-title="Stable tokens"
          :current-price="rates.scRate"
          :circulating-supply="states.stableCoinAmount"
          current-ratio-val="6.44"
          is-rsv=false
        />
      </b-col>
      <b-col lg="4" md="12" sm="12">
        <SigGenCard
          id="id-sig-rsv"
          card-title="Reserve tokens"
          :current-price="rates.rcRate"
          :circulating-supply="states.reserveCoinAmount"
          current-ratio-val="800"
          is-rsv=true
          reserve-ratio="373"
        />
      </b-col>
      <b-col lg="4" md="12" sm="12">
        <SigGenCard
            id="id-sig-rsv"
            card-title="Base Currency Ada"
            :current-price="rates.pegRate"
            :circulating-supply="states.baseReserveAmount"
            current-ratio-val="800"
            is-rsv=false
        />
      </b-col>
    </b-row>
    <MyBalanceCard
        :sig-usd-val="stableTokenTotal"
        :sig-rsv-val="reserveTokenTotal"
        :erg-val="adaTotal"
    />
  </div>
</template>

<script>
import NavBar from "../base/NavBar";
import SigGenCard from "./SigGenCard";
import MyBalanceCard from "./MyBalanceCard";
import {mapGetters} from "vuex";
export default {
  name: "Dashboard",
  components: {MyBalanceCard, SigGenCard, NavBar},
  computed: {
    ...mapGetters({
      adaTotal: "getAdaTotal",
      stableTokenTotal: "getStableTokenTotal",
      reserveTokenTotal: "getReserveTokenTotal",
      rates: "getCurrentRates",
      states: "getCurrentState"
    })
  },
  created() {
    this.$http.post(
        `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/currentState`,"\"\""
    );
  }
}
</script>

<style scoped>
.header{
  margin: 48px 0;
  text-align: center;
}
.row{
  margin-bottom: 56px;
  row-gap: 28px;
}
</style>