<template>
  <div>
    <NavBar/>
    <b-row
        v-if="this.$store.state.contract === undefined ||
        this.$store.state.contract.funds === undefined ||
        this.$store.state.contract.funds.tokens === undefined ||
        this.$store.state.contract.funds.tokens.length===0">
      <b-col class="mt-5 pt-5 text-center">
        <h3 class="text-muted">Nothing to see here</h3>
      </b-col>
    </b-row>
    <div v-else class="container-fluid mt-4">
      <b-row>
        <b-col cols="12" lg="6" class="my-2" v-for="(token,index) in this.$store.state.contract.funds.tokens"
               :key="index">
          <b-card class="text-monospace">
            <b-card-text>
              <p><strong>Currency</strong> <span class="text-muted">{{ token.currency }}</span></p>
            </b-card-text>
            <b-card-text>
              <p><strong>Token</strong> <span class="text-muted">{{ token.name }}</span></p>
            </b-card-text>
            <b-card-text v-if="token.value===1">
              <strong class="badge badge-pill badge-secondary">NFT</strong>
            </b-card-text>

            <b-button-toolbar aria-label="Bid">
              <b-button-group size="sm" class="mr-1">
                <b-button v-b-modal="'a'+token.index" variant="primary" @click="tokenClicked = token;">
                  Start Auction
                </b-button>
              </b-button-group>
              <b-input-group size="sm" class="mr-1">
                <b-button v-b-modal="'s'+token.index" variant="success" @click="tokenClicked = token;">
                  Sell
                </b-button>
              </b-input-group>
            </b-button-toolbar>
          </b-card>
        </b-col>
      </b-row>
    </div>
    <b-modal size="lg" :id="'s'+tokenClicked.index" title="Sell Token" ref="my_modal" hide-footer>
      <b-form @submit.prevent="onSell()">
        <div class="py-1"><strong>Policy : </strong><span class="text-sm-left">{{ tokenClicked.currency }}</span></div>
        <div class="py-1"><strong>Token:</strong>&nbsp; {{ tokenClicked.name }}</div>
        <b-form-input class="my-3" id="amount" v-model="amount" ref="input_number"
                      placeholder="Enter Sell price in Lovelace"
                      required>
        </b-form-input>
        <b-button class="my-2" type="submit" variant="success">Sell</b-button>
      </b-form>
    </b-modal>
    <b-modal size="xl" :id="'a'+tokenClicked.index" title="Edit Auction Details" ref="auction_modal" hide-footer>
      <b-container>
        <b-row class="my-1">
          <b-col sm="3"><label for="input-policy"><strong>Policy</strong></label></b-col>
          <b-col sm="9">
            <b-form-input id="input-policy" v-model="tokenClicked.currency" :state="null"
                          placeholder="No validation" disabled></b-form-input>
          </b-col>
        </b-row>
        <b-row class="my-1">
          <b-col sm="3"><label for="input-token"><strong>Token Name</strong></label></b-col>
          <b-col sm="9">
            <b-form-input id="input-token" v-model="tokenClicked.name" :state="null"
                          placeholder="No validation" disabled></b-form-input>
          </b-col>
        </b-row>
        <b-row class="my-1">
          <b-col sm="3"><label for="input-bid">Starting Bid</label></b-col>
          <b-col sm="9">
            <b-form-input id="input-bid" v-model="auction.apMinBid.value" :state="null"
                          placeholder="No validation"></b-form-input>
          </b-col>
        </b-row>
        <b-row class="my-1">
          <b-col sm="3"><label for="input-min-bid">Minimum Bid Increment</label></b-col>
          <b-col sm="9">
            <b-form-input id="input-min-bid" v-model="auction.apMinIncrement" :state="null"
                          placeholder="No validation"></b-form-input>
          </b-col>
        </b-row>
        <!--        <b-row class="my-1">-->
        <!--          <b-col sm="3"><label for="input-start">Start Time</label></b-col>-->
        <!--          <b-col sm="9">-->
        <!--            <b-form-input id="input-start" v-model="auction.apStartTime" :state="null"-->
        <!--                          placeholder="No validation"></b-form-input>-->
        <!--          </b-col>-->
        <!--        </b-row>-->
        <!--        <b-row class="my-1">-->
        <!--          <b-col sm="3"><label for="input-end">End Time</label></b-col>-->
        <!--          <b-col sm="9">-->
        <!--            <b-form-input id="input-end" v-model="auction.apEndTime" :state="null"></b-form-input>-->
        <!--          </b-col>-->
        <!--        </b-row>-->
        <b-row class="my-1">
          <b-col sm="3"><label for="input-start">Start Time</label></b-col>
          <b-col sm="9">
            <datetime id="input-start" type="datetime" v-model="auction.apStartTime"></datetime>
            <p>Slot Number: {{startSlotNo}}</p>
          </b-col>
        </b-row>
        <b-row class="my-1">
          <b-col sm="3"><label for="input-end">End Time</label></b-col>
          <b-col sm="9">
            <datetime id="input-end" type="datetime" v-model="auction.apEndTime"></datetime>
            <p>Slot Number: {{endSlotNo}}</p>
          </b-col>
        </b-row>
        <b-row class="mt-5">
          <b-col></b-col>
          <b-col sm="9" class="text-right">
            <b-button @click="onPlaceOnAuction" variant="success">Start Auction</b-button>
          </b-col>
        </b-row>
      </b-container>
    </b-modal>
  </div>
</template>

<script>
import {Datetime} from 'vue-datetime';
import NavBar from "@/components/base/NavBar";
import moment from "moment";

export default {
  name: "BuySell",
  components: {NavBar, datetime: Datetime},
  computed: {
    startSlotNo() {
      let startTime = moment(this.auction.apStartTime).format().replace('.000Z', 'Z')
      startTime = parseInt(moment(startTime).valueOf()) / 1000
      const diff = startTime - 1596059091
      return diff
    },
    endSlotNo() {
      let endTime = moment(this.auction.apEndTime).format().replace('.000Z', 'Z')
      endTime = parseInt(moment(endTime).valueOf()) / 1000
      const diff = endTime - 1596059091
      return diff
    }
  },
  methods: {
    onSell() {
      const amount = this.toLovelace(this.$refs.input_number.value)
      if (amount === 0 || !amount) {
        this.$task.errorMessage("Couldn't parse into Lovelace Value")
        return;
      }
      this.$task.do(
          this.$http.post(
              `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/sell`
              , [{
                "spItems": [{currency: this.tokenClicked.currency, token: this.tokenClicked.name, value: 1}],
                "spSaleType": "Primary",
                "spCost": {currency: "", token: "", value: amount}
              }]
          ).then(() => {
                this.$task.infoMessage("Transaction Submitted. NFT placed in marketplace")
                this.$bvModal.hide('' + this.tokenClicked)
              }
          )
      )
    },
    onPlaceOnAuction() {
      let startTime = moment(this.auction.apStartTime).format().replace('.000Z', 'Z')
      startTime = parseInt(moment(startTime).valueOf()) / 1000
      // console.log('Post Start Time: ' + startTime)

      let endTime = moment(this.auction.apEndTime).format().replace('.000Z', 'Z')
      endTime = parseInt(moment(endTime).valueOf()) / 1000
      // console.log('Post End Time: ' + endTime)

      this.$task.do(
          this.$http.post(
              `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/startAuction`,
              [{
                apValue: [{
                  currency: this.tokenClicked.currency,
                  token: this.tokenClicked.name,
                  value: 1
                }],
                apMinBid: {
                  currency: this.auction.apMinBid.currency,
                  token: this.auction.apMinBid.token,
                  value: this.toLovelace(this.auction.apMinBid.value)
                },
                apMinIncrement: this.toLovelace(this.auction.apMinIncrement),
                apStartTime: {getPOSIXTime: startTime},
                apEndTime: {getPOSIXTime: endTime}
              }]
          ))
    },
    toLovelace: (a) => {
      try {
        a = a.toUpperCase().replaceAll('_', '').replaceAll(' ', '')
        if (a.endsWith('A')) {
          return parseInt(a.substring(0, a.length - 1), 10) * 1000000
        } else if (a.endsWith("ADA")) {
          return parseInt(a.substring(0, a.length - 3), 10) * 1000000
        }
        return parseInt(a, 10)
      } catch (e) {
        return 0
      }
    }
  },
  data: () => {
    const startTimeString = moment.utc(Math.floor(1596059091 * 1000)).format().replace('Z', '.000Z')
    const endTimeString = moment.utc(Math.floor(1596059091 * 1000 + 90 * 1000)).format().replace('Z', '.000Z')

    console.log('Get Start Time: ' + startTimeString)
    console.log('Get End Time: ' + endTimeString)

    return {
      amount: "",
      tokenClicked: {
        currency: "",
        name: "",
        index: 0
      },
      selectedIndex: 0,
      auction: {
        "apMinBid": {
          "currency": "",
          "token": "",
          "value": "2 Ada"
        },
        "apMinIncrement": "2 Ada",
        "apStartTime": startTimeString,
        "apEndTime": endTimeString,
      }
    }
  }
}
</script>

<style scoped>

</style>
