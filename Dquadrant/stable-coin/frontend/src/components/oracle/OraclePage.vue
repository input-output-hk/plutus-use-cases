<template>
  <div class="constraint-1080">
    <NavBar></NavBar>
      <div class="header h-centered-flex">
        <h1>Update Exchange rate Using Oracles</h1>
      </div>

    <b-tabs content-class="mt-3">
      <b-tab title="Price Oracle" active>

        <OracleCard
            id="price-oracle-key"
            title-text="Update Price of 1 USD in Ada"
            submit-text="Update Price"
            :conversion-rate="stable_mint_to_ada"
            conversion-from-text="Stable"
            conversion-to-text="Ada"
            on-change-input=""
            @submit-action="onSubmitOracleValue"
            input-label-text="Enter amount in ada for 1 USD here"
            input-helper-text="* Note this if for demo purpose of showing fluctuations in price of usd to ada rate."
            step="0.000001"
        />
      </b-tab>
    </b-tabs>

  </div>
</template>

<script>
import OracleCard from "../oracle/OracleCard";
import {mapActions, mapGetters} from "vuex";
import NavBar from "../base/NavBar";
import {toLovelace} from "../../util/stringUtils";


export default {
  name: "OraclePage",
  components: {
    NavBar,
    OracleCard
  },

  // only mapState and mapGetters
  computed: {
    ...mapGetters([
      "stable_mint_to_ada",
      "stable_redeem_to_ada",
    ])
  },

  // only mapMutations and mapActions
  methods: {
    ...mapActions([
      "onSubmitOracleValue",
      "onSubmitFeeValue",

    ]),
    onSubmitOracleValue(inputVal) {
      this.$task.start()
      let parsedVal = parseFloat(inputVal);
      let newOracleVal = toLovelace(parsedVal);
      console.log(newOracleVal);
      this.$http.post(
          `/instance/${this.$store.state.contract.oracleContractInstanceId}/endpoint/update`
          ,
          parseInt(newOracleVal)
          ,
          {
            headers: {
              // Overwrite Axios's automatically set Content-Type
              'Content-Type': 'application/json'
            }
          }
      ).then(() => {
            this.$task.infoMessage("Transaction Submitted. Wait for it to get confirmed")
            this.$task.complete();
          }
      );
    },
  }
}
</script>

<style scoped>
.header{
  margin: 48px 0;
}
</style>