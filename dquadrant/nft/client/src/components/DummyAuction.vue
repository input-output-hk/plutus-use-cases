<template>
  <div>
    <NavBar/>
    <b-row v-if="items === undefined || items.length===0">
      <b-col class="mt-5 pt-5 text-center">
        <h3 class="text-muted">Nothing to see here</h3>
      </b-col>
    </b-row>
    <div v-else class="mt-4 container-fluid">
      <b-row>
        <b-col v-for="(item, i) in items" :key="item.arBidder.bBidReference.txOutRefId.getTxId"
               cols="12" lg="6" class="my-2">
          <div v-for="(content, index) in item.arValue" :key="index">
            <b-card :title="content.token" :sub-title="content.value === 1 ? 'NFT' : content.value"
                    class="text-monospace">
              <b-card-text>
                <p><strong>Policy</strong> <span class="text-muted">{{ content.currency }}</span></p>
              </b-card-text>

              <b-card-text>
                <p><strong>Owner</strong> <span class="text-muted">{{ item.arOwner.getPubKeyHash }}</span></p>
                <div v-if="item.arOwner.getPubKeyHash!==item.arBidder.bPubKeyHash.getPubKeyHash">
                  <p>
                    <strong>Bidder</strong> <span class="text-muted">{{
                      item.arBidder.bPubKeyHash.getPubKeyHash
                    }}</span>
                  </p>
                  <p class="d-flex justify-content-between">
                    <strong>Last Bid</strong> <span class="text-muted">{{ item.arBidder.bBid.toLocaleString() }}</span>
                  </p>
                </div>
                <div class="text-muted pb-2" v-else>
                  <p>No Bids yet</p>
                </div>
                <div class="d-flex justify-content-between">
                  <strong>Start Time</strong>
                  <p class="text-muted">{{ item | parseStartTime }}</p>
                </div>
                <div class="d-flex justify-content-between">
                  <strong>End Time</strong>
                  <p class="text-muted">{{ item | parseEndTime }}</p>
                </div>
              </b-card-text>
              <b-form-input class="mb-2" type="number" v-model.number="item.minNewBid"
                            v-on:change="onBidChange(i)"></b-form-input>
              <b-button-toolbar aria-label="Bid">
                <b-button-group size="sm" class="mr-1">
                  <b-button variant="primary" @click="onBid(item)" :disabled="item.arBidder.bBid >= item.minNewBid">
                    Bid ({{ item.minNewBid.toLocaleString() }})
                  </b-button>
                </b-button-group>
                <b-input-group size="sm" class="mr-1">
                  <b-button variant="success" @click="onBidIncrease(i)">
                    <BIconPatchPlus></BIconPatchPlus>
                  </b-button>
                </b-input-group>
                <b-input-group size="sm">
                  <b-button variant="warning" @click="onBidDecrease(i)"
                            :disabled="item.arBidder.bBid >= item.minNewBid">
                    <BIconPatchMinus></BIconPatchMinus>
                  </b-button>
                </b-input-group>
                <b-input-group  @click="claim(item)" class="float-right ml-5 pl-5">
                  <b-button> Claim </b-button>
                </b-input-group>
              </b-button-toolbar>

            </b-card>
          </div>
        </b-col>
      </b-row>
    </div>
  </div>
</template>

<script>
import NavBar from "./base/NavBar";
// import moment from "moment";
import {parseTimeStampToDate} from '@/util/util';
const buffer=require("buffer")

export default {
  name: "DummyAuction",
  components: {NavBar},
  created() {
    this.items = []
    if (this.instanceId) {
      this.refresh()
    }
  },
  destroyed() {
    if(typeof this.timeoutHandle === "number")
      clearTimeout(this.timeoutHandle)
  },
  computed: {
    instanceId() {
      return this.$store.state.contract.instance.cicContract.unContractInstanceId
    },
    lastResponse() {
      return this.$store.state.contract.lastObservable
    }
  },
  filters: {
    parseStartTime(item) {
      let timeStamp;
      if (item.arDuration !== undefined && item.arDuration.length !== 0) {
        timeStamp = item.arDuration[0].contents
        // timeStamp = moment(timeStamp).format("MM/DD/YYYY hh:mm a")
        timeStamp = parseTimeStampToDate(timeStamp)
      } else {
        timeStamp = 'Start Time Unspecified'
      }
      return timeStamp
    },
    parseEndTime(item) {
      let timeStamp;
      if (item.arDuration !== undefined && item.arDuration.length !== 0) {
        timeStamp = item.arDuration[1].contents
        // timeStamp = moment(timeStamp).format("MM/DD/YYYY hh:mm a")
        timeStamp = parseTimeStampToDate(timeStamp)
      } else {
        timeStamp = 'End Time Unspecified'
      }
      return timeStamp
    },
  },
  methods: {
    claim(item){
      this.$task.do(
        this.$http.post(`instance/${this.instanceId}/endpoint/claim`,{
          references: [item.arBidder.bBidReference],
          ignoreUnClaimable: false
        }).then(()=>this.$task.infoMessage("Submitted Claim operation"))
      )
    },
    onBid(item) {
      this.flight = true
      this.$task.do(
          this.$http.post(`instance/${this.instanceId}/endpoint/bid`, {
            "ref": item.arBidder.bBidReference,
            "bidValue": [
              {
                "currency": item.arMinBid.currency,
                "token": item.arMinBid.token,
                "value": item.minNewBid
              }
            ]
          }).then(() => this.$task.infoMessage("Transaction Submitted."))
      )
        this.$http.post(`instance/${this.instanceId}/endpoint/list`, {
          lmUtxoType: "MtAuction"
        })
      this.$store.dispatch('updateAuctionItems', this.items)
    },
    refresh() {
      this.flight = true
      this.$http.post(`instance/${this.instanceId}/endpoint/list`, {
        lmUtxoType: "MtAuction"
      })
      if (this.items.length === 0)
        this.timeoutHandle = setTimeout(this.refresh, 5000)
      else {
        this.$store.dispatch('updateAuctionItems', this.items)
        clearTimeout(this.timeoutHandle)
      }
    },
    onBidIncrease(index) {
      this.$store.dispatch('updateAuctionItems', this.items)
      const item = this.$store.state.auctionItems[index]
      item.minNewBid += item.arMinIncrement
      this.$store.dispatch('updateAuctionItemBidPrice', {item, index})
    },
    onBidDecrease(index) {
      this.$store.dispatch('updateAuctionItems', this.items)
      const item = this.$store.state.auctionItems[index]
      if (item.arBidder.bBid + item.arMinIncrement < item.minNewBid) {
        item.minNewBid -= item.arMinIncrement
        this.$store.dispatch('updateAuctionItemBidPrice', {item, index})
      }
    },
    onBidChange(index) {
      this.$store.dispatch('updateAuctionItems', this.items)
      const item = this.$store.state.auctionItems[index]
      if (item !== undefined && item.arBidder.bBid >= item.minNewBid) {
        item.minNewBid = item.arBidder.bBid
        this.$store.dispatch('updateAuctionItemBidPrice', {item, index})
      }
    }
  },

  watch: {
    lastResponse(newVal) {
      if (this.flight) {
        if (newVal.getTxId) {
          this.$task.successMessage("Transaction Confirmed :" + newVal.getTxId)
        } else {
          const items = JSON.parse(JSON.stringify(newVal))
          items.forEach(x => {
            x.minNewBid = x.arBidder.bBid === 0 ? x.arMinBid.value : (x.arBidder.bBid + x.arMinIncrement)
            x.arValue.forEach(v=>{v.token=new buffer.Buffer(v.token,"hex").toString()})
          })
          this.items = items
        }
        this.flight = false
      }
    },
    instanceId(newVal, oldVal) {
      if (oldVal === undefined) {
        this.refresh()
      }
    }
  },
  data: () => {
    return {
      flight: false,
      timeoutHandle: undefined,
      items: [
        {
          "arMarketFee": 5000000,
          "arStartTime": [
            {
              "contents": 0,
              "tag": "Finite"
            },
            true
          ],
          "arMinIncrement": 2000000,
          "arValue": [
            {
              "currency": "5e12804044e10921ed3a786b8602ab844eeedfc55dbae30374b287a4bb1e554c",
              "token": "aa",
              "value": 1
            }
          ],
          "arOwner": {
            "getPubKeyHash": "21fe31dfa154a261626bf854046fd2271b7bed4b6abe45aa58877ef47f9721b9"
          },
          "arBidder": {
            "bBid": 32,
            "bBidReference": {
              "txOutRefIdx": 1,
              "txOutRefId": {
                "getTxId": "874c8b1f8682bf22bd35a13b7d5a5fadb8f7aa775f5dee10bb022c019b32abe6"
              }
            },
            "bPubKeyHash": {
              "getPubKeyHash": "21fe31dfa154a261626bf854046fd2271b7bed4b6abe45aa58877ef47f9721b9"
            }
          },
          "arEndTime": [
            {
              "contents":1624744416,
              "tag": "Finite"
            },
            false
          ],
          minNewBid: 32,
          "arMinBid": {
            "currency": "",
            "token": "",
            "value": 10000000
          }
        }
      ]
    }
  }
}
</script>

<style scoped>

</style>
