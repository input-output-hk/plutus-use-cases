import Vue from 'vue';
import Vuex from 'vuex';

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
        cicContract:{
          unContractInstanceId: undefined
        },
      },
      funds:{
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
  }
});
