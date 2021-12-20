import Vue from "vue";
import {store} from "./vuex";

let task
task = Vue.prototype.$task = {
  // eslint-disable-next-line no-unused-vars
  error(error = null) {
    task.thrown(error)
    task.complete(false)
  },
  do(_task){
    task.start()
    return _task.then(task.complete).catch(task.error)
  },
  errorMessage(message) {
    store.state.alert.variant = 'danger';
    store.state.alert.countDown = 7;
    store.state.alert.message = message
  },
  successMessage(message) {
    store.state.alert.message = message;
    store.state.alert.variant = 'success';
    store.state.alert.countDown = 4;
  },
  warnMessage(message){
    store.state.alert.message = message;
    store.state.alert.variant = 'warning';
    store.state.alert.countDown = 4;
  },
  infoMessage(message) {
    store.state.alert.message = message;
    store.state.alert.variant = 'info';
    store.state.alert.countDown = 4;
  },
  start() {
    if (!store.state.progress.ongoing) {
      store.state.progress.status = true
    }
    store.state.progress.ongoing++;
    store.commit('setProgress', {
      animate: true,
      status: true
    })
  },
  success(message = null) {
    if (message) {
      task.successMessage(message)
    }
    task.complete(true)
  },
  complete(status = true) {
    store.state.progress.ongoing--;
    if (!store.state.progress.ongoing) {
      store.state.progress.value = 0;
      store.state.progress.status &&= status
      store.commit('setProgress', {
        animate: false,
        status: status
      })
    }
  },
  thrown(error) {
    console.error(error)
    const alert = store.state.alert
    if (error) {
      if (error.response) {
        if (error.response.message) {
          alert.message = error.response.message
        } else if (error.response.data.message) {
          alert.message = error.response.data.message
        } else if(error.message) {
          if(error.response.data){
            alert.message=error.message + " -- "+error.response.data
          }else{
            alert.message=error.message
          }
        }
        else{
          alert.message = "Something went wrong"
        }
      } else if (error.message) {
        alert.message = error.message;
      } else {
        alert.message = error
      }
    } else {
      alert.message = "Something went wrong"

    }
    alert.variant = 'danger';
    alert.countDown = 7;
  }
}

