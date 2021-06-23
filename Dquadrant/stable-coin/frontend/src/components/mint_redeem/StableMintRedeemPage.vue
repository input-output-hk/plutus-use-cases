<template>
  <b-container>
    <b-row align-content="center">
      <b-col xl="6" lg="6" md="12" sm="24">
        <MintRedeemCard
            id="stable-mint-key"
            title-text="Mint Stable coin"
            submit-text="Mint Stable coin"
            :conversion-rate="stable_mint_to_ada"
            conversion-from-text="Stable"
            conversion-to-text="Ada"
            on-change-input=""
            @submit-action="onMintStableCoin"
        />
      </b-col>
      <b-col xl="6" lg="6" md="12" sm="24">
        <MintRedeemCard
            id="stable-redeem-key"
            title-text="Redeem Stable coin"
            submit-text="Redeem Stable coin"
            conversion-rate="reserve_mint_to_ada"
            conversion-from-text="Stable"
            conversion-to-text="Ada"
            on-change-input=""
            @submit-action="onRedeemStableCoin"
        />
      </b-col>
    </b-row>
  </b-container>
</template>

<script>
import MintRedeemCard from "./MintRedeemCard";
import {mapActions, mapGetters} from "vuex";

export default {
  name: "StableMintRedeemPage",
  components: {
    MintRedeemCard
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
      "mintStableCoin",
      "redeemStableCoin",
    ]),
    onMintStableCoin(inputVal,rateNume,rateDeno) {
      console.log(rateNume+rateDeno)
      this.$task.start()
      this.$http.post(
          `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/mintStableCoin`
          ,
          {
            "rateNume": parseInt(rateNume),
            "rateDeno": parseInt(rateDeno),
            "tokenAmount": parseInt(inputVal)
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
    onRedeemStableCoin(inputVal,rateNume,rateDeno) {
      console.log(rateNume+rateDeno)
      this.$task.start()
      this.$http.post(
          `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/redeemStableCoin`
          ,
          {
            "rateNume": parseInt(rateNume),
            "rateDeno": parseInt(rateDeno),
            "tokenAmount": parseInt(inputVal)
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
.row {
  row-gap: 12px;
}

.col {
  padding: 0 6px;
}
</style>