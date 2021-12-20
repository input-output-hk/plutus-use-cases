<template>
  <div class="card">
    <h6 class="card-title">{{titleText}}</h6>
    <div class="amount-input-form">
      <b-form-group label="" v-slot="{ ariaDescribedby }">
        <b-form-radio v-model="selected" :aria-describedby="ariaDescribedby" name="pause" value="pause">Pause</b-form-radio>
        <b-form-radio v-model="selected" :aria-describedby="ariaDescribedby" name="resume" value="resume">Resume</b-form-radio>
      </b-form-group>
    </div>

    <span class="text-muted">{{inputHelperText}}</span>
    <div class="action-wrapper">
      <MyButton v-bind:modal-id="id"
                show-loading="false"
                :disabled="selected.length===0"
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
  name: "ContractStatus",
  components: {
    MyButton
  },
  props: ["id", "titleText", "submitText",
    "conversionRate", "conversionFromText",
    "conversionToText", "inputLabelText", "inputHelperText","step"],
  data(){
    return {
      selected: ''
    }
  },
  methods: {
    onSubmit(){
      console.log("in card: "+ this.selected)
      this.$emit("submit-action",  this.selected)
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
  /*width: 768px;*/
  /*padding: 28px;*/
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

/*@media(max-width: 868px){*/
/*  .card{*/
/*    width: 100%;*/
/*    padding: 16px;*/
/*  }*/
/*}*/

@media(max-width: 576px){
  .modal-dialog{
    max-width: 768px;
  }
}
</style>