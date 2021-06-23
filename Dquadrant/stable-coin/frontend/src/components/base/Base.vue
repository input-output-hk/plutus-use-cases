<template>
  <div>
    <header>
      <b-progress :max="100" height="0.2rem" animated>
        <b-progress-bar ref="progress" :value="this.loadingProgress" :variant="variant"></b-progress-bar>
      </b-progress>
    </header>
    <router-view></router-view>
    <b-alert
      class="position-fixed fixed-bottom m-0 rounded-0"
      style="z-index: 2000;"
      :variant="$store.state.alert.variant"
      :show="$store.state.alert.countDown"
      @dismissed="$store.state.alert.countDown=0"
      @dismiss-count-down="alertCountdownChanged"
      dismissibe
    >
      {{ $store.state.alert.message }}
    </b-alert>
  </div>
</template>

<script>

export default {
  name: "Base",
  mounted() {
    this.$task.do(
      this.$http.get('/instances').then(x => {
        if (x.data.length === 0) {
          throw Error("PAB responded with empty list of contract instances")
        }
        const sorted = x.data.sort((a, b) => a.cicWallet.getWallet > b.cicWallet.getWallet)
        this.$store.state.contract.instances = sorted
        this.$store.state.contract.instance = sorted[0]
      }).then(this.refreshStatus)
    )
  },
  methods: {

    refreshStatus() {
      return this.$http.get(
        `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/status`,
      ).then((x) => {
        this.$store.state.contract.instance.status=x.data.cicCurrentState
        this.timeoutHandle = setTimeout(this.refreshStatus, 500)
      })
    },
    alertCountdownChanged(newValue) {
      this.alert.countDown = newValue
    },
    changeProgress: function () {
      if (this.increasing) {
        if (this.loadingProgress > 85) {
          this.increasing = false
          this.loadingProgress += 5;
        } else {
          this.loadingProgress += 12;
        }
      } else {
        this.loadingProgress -= 5
        if (this.loadingProgress < 70) {
          this.increasing = true
        } else {
          this.loadingProgress -= 5;
        }
      }
    },
  },
  computed: {
    activeInstance() {
      return this.$store.state.contract.instance
    },
    animate() {
      return this.$store.state.progress.animate
    },
    progressStatus() {
      return this.$store.state.progress.status
    }
  },
  watch: {
    animate(newValue, oldValue) {
      if (newValue) {
        if (!oldValue) {
          this.variant = 'primary'
          this.$refs['progress']._vnode.elm.classList.remove('no-transition')
          this.loadingProgress = 20
          const func = () => {
            this.changeProgress()
            this.timeout = setTimeout(func, 100)
          }
          this.timeout = setTimeout(func, 200)
        }
      } else if (oldValue) {
        if (this.progressStatus) {
          this.variant = 'success'
        }
        this.loadingProgress = 100
        clearTimeout(this.timeout)
        setTimeout(() => {
          this.$refs['progress']._vnode.elm.classList.add('no-transition')
          this.loadingProgress = 0
        }, 700)

      }
    },
    progressStatus(newValue) {
      if (newValue)
        this.variant = 'success';
      else
        this.variant = 'danger';
    },
    activeInstance(newVal) {
      console.log(newVal)
      this.$store.state.contract.logs = []
      this.$store.state.contract.funds = []
      this.$store.state.contract.responses = []
    }
  },
  data: function () {
    return {
      alert: {
        countDown: 0,
        show: false,
        variant: 'warning',
        message: 'Syncing data..'
      },
      variant: "success",
      loadingProgress: 0,
      timeoutHandle: undefined
    }
  }
}
</script>
<style scoped>
header {
  position: fixed;
  top: 0;
  width: 100vw;
  z-index: 1050;
}

.main-view {
  position: static;
  left: auto;
  z-index: 5;
}

.no-transition {
  -webkit-transition: none !important;
  -moz-transition: none !important;
  -o-transition: none !important;
  -ms-transition: none !important;
  transition: none !important;
}
</style>
