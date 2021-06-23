<template>
  <div class="card sketchy-box-shadow">
    <h6 class="card-title">{{titleText}}</h6>
    <div class="amount-input-form">
      <b-form-input v-model="inputVal"
                    placeholder="Enter amount in lovelace for 1 USD here"
                    type="number"
                    required
      >
      </b-form-input>
    </div>

    <span class="text-muted">* Note this if for demo purpose of showing fluctuations in price of usd to ada rate.</span>
    <div class="action-wrapper">
      <MyButton v-bind:modal-id="id"
                show-loading="false"
                :disabled="inputVal.length===0"
                :btn-text="submitText"
                @click-btn="onSubmit"
      />
    </div>
  </div>
</template>

<script>
import MyButton from "../base/MyButton";
import {mapGetters} from "vuex";

export default {
  name: "OracleCard",
  components: {
    MyButton
  },
  props: ["id", "titleText", "submitText",
    "conversionRate", "conversionFromText",
    "conversionToText"],
  data(){
    return {
      inputVal: "",
      rateNume: "1",
      rateDeno: "1",
      convertedAda: "",
      feeAda: ""
    }
  },
  methods: {
    onSubmit(){
      console.log("in card: "+ this.inputVal)
      this.$emit("submit-action",  this.inputVal,this.rateNume,this.rateDeno)
    }
  },
  computed: mapGetters({
    // selected contract instance watched from navbar in Base.vue
    instance: "getInstance"
  })
}
</script>

<style scoped>
.card{
  padding: 28px;
  background: white;
  border-radius: 12px;
  border: none;
}
.card-title{
  color: #484848;
  font-weight: normal;

}
.amount-input-form{
  margin: 12px 0;
}
.amount-input{
  min-height: 50px;
  background: #f4f4f4;
  border: none;
  border-radius: 8px;
  width: 100%;
  padding: 12px;
}
.amount-input::placeholder{
  font-size: 18px;
  font-weight: normal;
}
.text-helper{
  color: #f28e38;
  display: block;
  margin: 12px 0;
}
.conversion-info{
  margin: 8px 0;
}
.text-conversion{
  color: #484848;
  display: block;
  width: 100%;
  font-size: 18px;

}
.action-wrapper{
  margin: 12px 0;
}
.modal-content{
  padding: 24px;
  border:none;
}
.modal-footer{
  border: none;
}
@media(max-width: 576px){
  .modal-dialog{
    max-width: 768px;
  }
}
</style>