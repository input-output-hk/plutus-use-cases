<template>
  <b-container>
    <b-row align-content="center" >
      <b-col xl="6" lg="6" md="12" sm="24">
        <MintRedeemCard
            id="reserve-mint-key"
            title-text="Mint Reserve coin"
            submit-text="Mint Reserve coin"
            :conversion-rate="reserve_mint_to_ada"
            conversion-from-text="Reserve"
            conversion-to-text="Ada"
            on-change-input=""
            @submit-action="onMintReserveCoin"

        />
      </b-col>
      <b-col xl="6" lg="6" md="12" sm="24">
        <MintRedeemCard
            id="reserve-redeem-key"
            title-text="Redeem Reserve coin"
            submit-text="Redeem Reserve coin"
            :conversion-rate="reserve_redeem_to_ada"
            conversion-from-text="Reserve"
            conversion-to-text="Ada"
            on-change-input=""
            @submit-action="onRedeemReserveCoin"
        />
      </b-col>
    </b-row>
  </b-container>
</template>

<script>
import MintRedeemCard from "./MintRedeemCard";
import {mapActions, mapGetters} from "vuex";

export default {
  name: "ReserveMintRedeemPage",
  components:{
    MintRedeemCard
  },

  // only mapState and mapGetters
  computed:{
    ...mapGetters([
      "reserve_mint_to_ada",
      "reserve_redeem_to_ada"
    ])
  },

  // only mapMutations and mapActions
  methods: {
    ...mapActions([
      "mintReservedCoin",
      "redeemReservedCoin"
    ]),
    onMintReserveCoin(inputVal,rateNume,rateDeno){
      console.log(rateNume+rateDeno)
      this.$task.start()
      this.$http.post(
          `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/mintReserveCoin`
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
      );    },
    onRedeemReserveCoin(inputVal,rateNume,rateDeno){
      console.log(rateNume+rateDeno)
      this.$task.start()
      this.$http.post(
          `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/redeemReserveCoin`
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
      );    }
  }
}
</script>

<style scoped>
.row{
    row-gap: 12px;
}
.col{
  padding: 0 6px;
}
</style>