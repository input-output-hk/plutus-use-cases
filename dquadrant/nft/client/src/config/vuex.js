import Vue from "vue";
import Vuex from "vuex";

Vue.use(Vuex);

export const store = new Vuex.Store({
    state: {
        alert: {
            message: "",
            variant: "",
            countDown: 0
        },
        progress: {
            ongoing: 0,
            value: 0,
            animate: false,
            status: true
        },
        contract: {
            activeIndex: 0,
            instance: {
                cicContract: {
                    unContractInstanceId: undefined
                }
            },
            funds: {
                ada: undefined,
                tokens: []
            },
            lastObservable: undefined,
            status: {
                logs: undefined,
                observableState: []
            },
            instances: []
        }
    },
    actions: {
        updateContractActiveIndex(context, index) {
            context.commit('setContractActiveIndex', index)
        },
        updateContractStatus(context, status) {
            context.commit('setContractStatus', status)
        },
        updateContractInstances(context, instances) {
            context.commit('setContractInstances', instances)
        },
        updateContractInstance(context, instance) {
            context.commit('setContractInstance', instance)
        },
        updateContractFunds(context, funds) {
            context.commit('setContractFunds', funds)
        },
        updateContractLastObservable(context, lastObservable) {
            context.commit('setContractLastObservable', lastObservable)
        }
    },
    mutations: {
        setProgress(state, payload) {
            // mutate state
            if (!payload.animate) {
                this.state.progress.status = payload.status;
            }
            this.state.progress.animate = payload.animate;
        },
        setContractActiveIndex(state, index) {
            Vue.set(state.contract, 'activeIndex', index)
        },
        setContractStatus(state, status) {
            Vue.set(state.contract, 'status', status)
        },
        setContractInstances(state, instances) {
            Vue.set(state.contract, 'instances', instances)
            if (instances.length > 0)
                Vue.set(state.contract, 'instance', instances[0])
            if (instances.length === 0)
                Vue.set(state.contract, 'instance', {
                    cicContract: {
                        unContractInstanceId: undefined
                    }
                })
        },
        setContractInstance(state, instance) {
            Vue.set(state.contract, 'instance', instance)
        },
        setContractFunds(state, funds) {
            Vue.set(state.contract, 'funds', funds)
        },
        setContractLastObservable(state, lastObservable) {
            Vue.set(state.contract, 'lastObservable', lastObservable)
        }
    }
});
