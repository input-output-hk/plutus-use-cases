<template>
  <b-container>
    <b-row align-content="center" >
      <b-col xl="6" lg="6" md="12" sm="24">
        <MintRedeemCard
            id="reserve-mint-key"
            title-text="Mint Reserve coin"
            submit-text="Mint Reserve coin"
            :conversion-rate="rates.rcRate"
            conversion-from-text="Reserve"
            conversion-to-text="Ada"
            ada-get = "Total ada required to pay"
            on-change-input=""
            @submit-action="onMintReserveCoin"
            :fee-value="getFeeValue()"
        />
      </b-col>
      <b-col xl="6" lg="6" md="12" sm="24">
        <MintRedeemCard
            id="reserve-redeem-key"
            title-text="Redeem Reserve coin"
            submit-text="Redeem Reserve coin"
            :conversion-rate="rates.rcRate"
            conversion-from-text="Reserve"
            conversion-to-text="Ada"
            ada-get = "Total ada you will receive"
            on-change-input=""
            @submit-action="onRedeemReserveCoin"
            :fee-value="getFeeValue()"
        />
      </b-col>
    </b-row>
  </b-container>
</template>

<script>
import MintRedeemCard from "./MintRedeemCard";
import {mapGetters} from "vuex";

export default {
  name: "ReserveMintRedeemPage",
  components:{
    MintRedeemCard
  },
  // only mapState and mapGetters
  computed:{
    ...mapGetters({
      rates: "getCurrentRates",
      states: "getCurrentState"
    })
  },

  // only mapMutations and mapActions
  methods: {
    //Calculate fee value by dividing ratios and multiply by 100 to get %
    getFeeValue(){
      return this.states.bankFee[0]/this.states.bankFee[1]
    },
    postStatusCall(){
      setTimeout(()=>{
        this.$http.post(
            `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/funds`,"\"\""
        ).finally(() => this.$http.post(
            `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/currentRates`,"\"\""
        )).finally(() => this.$http.post(
            `instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/currentState`,"\"\""
        ))
      }, 2000)
    },
    onMintReserveCoin(inputVal){
      this.$task.start()
      this.$http.post(
          `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/mintReserveCoin`
          ,
          {
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
          }).then(() => this.postStatusCall());
      },
    onRedeemReserveCoin(inputVal){
      this.$task.start()
      this.$http.post(
          `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/redeemReserveCoin`
          ,
          {
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
          }).then(() => this.postStatusCall());
    }
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