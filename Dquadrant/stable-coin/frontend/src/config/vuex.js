import Vue from 'vue';
import Vuex from 'vuex';
import {getCurrentStateAndRates, getTotalTokens} from "../util/arrUtil";

Vue.use(Vuex);

export const store = new Vuex.Store({
  state: {
    alert: {
      'message': "",
      'variant': "",
      'countDown': 0
    },
    progress: {
      ongoing: 0,
      value: 0,
      animate: false,
      status: true
    },
    contract: {
      activeIndex:0,
      instance: {
        cicDefinition:{
          tag: undefined
        },
        cicContract:{
          unContractInstanceId: undefined,
        },
        cicWallet:{
          getWallet: undefined
        }
      },
      oracleContractInstanceId:  undefined,
      feeContractInstanceId:  undefined,
      funds: {
        ada:undefined,
        tokens:[]
      },
      lastObservable:undefined,
      status:{
        logs:[],
        observableState:[]
      },
      instances: [],

    }
  },
  mutations: {
    setProgress(state, payload) {
      // mutate state
      if (!payload.animate) {
        this.state.progress.status = payload.status
      }
      this.state.progress.animate = payload.animate
    }
  },
  getters: {
    getAdaTotal: state => {
      return getTotalTokens(state.contract.status.observableState, "AdaToken", false)
    },
    getStableTokenTotal: state => {
      return getTotalTokens(state.contract.status.observableState, "StableToken")
    },
    getReserveTokenTotal: state => {
      return getTotalTokens(state.contract.status.observableState, "ReserveToken")
    },
    getCurrentRates: state => {
      return getCurrentStateAndRates(state.contract.status.observableState, "currentCoinsRates")
    },
    getCurrentState: state => {
      return getCurrentStateAndRates(state.contract.status.observableState, "currentCoinsState");
    }
  }
});
