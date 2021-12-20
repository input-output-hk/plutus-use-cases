<template>
  <div class="constraint-1080">
    <NavBar></NavBar>
      <div class="header h-centered-flex">
        <h1>Configure contract fees and status</h1>
      </div>

    <b-tabs content-class="mt-3">
      <b-tab title="Update Fee">

        <OracleCard
            id="fee-key"
            title-text="Update Fee charged in % for minting or redeeming coins"
            submit-text="Update Fee"
            :conversion-rate="stable_mint_to_ada"
            conversion-from-text="Stable"
            conversion-to-text="Ada"
            on-change-input=""
            @submit-action="onSubmitFeeValue"
            input-label-text="Enter % charge as fee eg: 1 "
            input-helper-text="* Note only owner wallet can update contract state."
            step="1"
        />
      </b-tab>

      <b-tab title="Update Contract Status">

        <ContractStatus
            id="status-key"
            title-text="Update contract status to paused state or resumed state"
            submit-text="Update Status"
            :conversion-rate="stable_mint_to_ada"
            conversion-from-text="Stable"
            conversion-to-text="Ada"
            on-change-input=""
            @submit-action="onUpdateStatus"
            input-label-text="Enter integer % charge as fee eg: 1 "
            input-helper-text=""
            step="1"
        />
      </b-tab>
    </b-tabs>

  </div>
</template>

<script>
import {mapActions, mapGetters} from "vuex";
import NavBar from "../base/NavBar";
import ContractStatus from "./ContractStatus";
import OracleCard from "../oracle/OracleCard";
import Fraction from "fraction.js";


export default {
  name: "OraclePage",
  components: {
    NavBar,
    ContractStatus,
    OracleCard,
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
    onSubmitFeeValue(inputVal) {
      this.$task.start()
      let floatInputVal = parseFloat(inputVal);
      let fraction = new Fraction(floatInputVal);
      this.$http.post(
          `/instance/${this.$store.state.contract.feeContractInstanceId}/endpoint/updateBankFee`
          ,
          {
            "percentNumerator": fraction.n,
            "percentDenominator": fraction.d,

          },
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
    onUpdateStatus(inputVal) {
      this.$task.start()
      this.$http.post(
          `/instance/${this.$store.state.contract.feeContractInstanceId}/endpoint/updateContractStatus`
          ,
          {
            "shouldPause": inputVal === "pause"
          },
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