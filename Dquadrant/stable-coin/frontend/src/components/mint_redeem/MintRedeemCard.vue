<template>
  <div class="card sketchy-box-shadow">
    <h6 class="card-title">{{ titleText }}</h6>
    <div class="amount-input-form">
      <b-form-input v-model="inputVal"
                    placeholder="Enter amount here"
                    type="number"
                    required
      >
      </b-form-input>
    </div>
    <span class="text-helper">{{ feeValue }}% fee is charged for currency conversion</span>
    <div class="conversion-info">
      <span class="text-conversion">
        Conversion rate ({{ conversionFromText }} -> {{ conversionToText }}) = {{ conversionRateTxt }}
      </span>
      <span class="text-conversion">
        {{ adaGet }} = {{ convertedAda }} Ada
      </span>
    </div>
    <div class="action-wrapper">
      <MyButton v-bind:modal-id="id"
                show-loading="false"
                :disabled="inputVal.length===0 || this.states.contractStatus !== 'Running' "
                :btn-text="submitText"
                @click-btn="onSubmit"


      />
    </div>
  </div>
</template>

<script>
import MyButton from "../base/MyButton";
import {mapGetters} from "vuex";
import {toAda} from "../../util/stringUtils";

export default {
  name: "MintRedeemCard",
  components: {
    MyButton
  },
  props: ["id", "titleText", "submitText",
    "conversionRate", "conversionFromText",
    "conversionToText", "adaGet", "feeValue"],
  data() {
    return {
      inputVal: "",
    }
  },
  methods: {
    onSubmit() {
      this.$emit("submit-action", this.inputVal)
    },
  },
  computed: {
    ...mapGetters({
      states: "getCurrentState"
    }),
    conversionRateTxt() {
      const adaVal = toAda(this.conversionRate)
      if (adaVal >= 1) {
        return `${adaVal} Ada`
      }
      return `${this.conversionRate} lovelace`
    },
    convertedAda() {
      const parsedInputVal = parseInt(this.inputVal)
      if (isNaN(parsedInputVal)) {
        return 0;
      } else {
        const feeValue = (this.states.bankFee[0] / this.states.bankFee[1])/100
        const totalLovelaceRequiredToPay = parseInt(this.inputVal) * this.conversionRate
        const totalLovelaceWithFee =
            this.titleText.includes("Redeem") ?
                totalLovelaceRequiredToPay - (totalLovelaceRequiredToPay * feeValue) :
                totalLovelaceRequiredToPay + (totalLovelaceRequiredToPay * feeValue)

        return toAda(totalLovelaceWithFee)
      }
    }
  }
}
</script>

<style scoped>
.card {
  padding: 28px;
  background: white;
  border-radius: 12px;
  border: none;
}

.card-title {
  color: #484848;
  font-weight: normal;

}

.amount-input-form {
  margin: 12px 0;
}

.amount-input {
  min-height: 50px;
  background: #f4f4f4;
  border: none;
  border-radius: 8px;
  width: 100%;
  padding: 12px;
}

.amount-input::placeholder {
  font-size: 18px;
  font-weight: normal;
}

.text-helper {
  color: #f28e38;
  display: block;
  margin: 12px 0;
}

.conversion-info {
  margin: 8px 0;
}

.text-conversion {
  color: #484848;
  display: block;
  width: 100%;
  font-size: 18px;

}

.action-wrapper {
  margin: 12px 0;
}

.modal-content {
  padding: 24px;
  border: none;
}

.modal-footer {
  border: none;
}

@media (max-width: 576px) {
  .modal-dialog {
    max-width: 768px;
  }
}
</style>