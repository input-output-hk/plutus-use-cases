<template>
  <b-navbar toggleable="md" class="shadow-sm pl-5 mb-4">
    <b-navbar-nav class="d-flex flex-row">
      <b-nav-item :active="$router.currentRoute.path.startsWith('/market')" to="/market">Market</b-nav-item>
      <b-nav-item class="ml-2" :active="$router.currentRoute.path.startsWith('/dummy-auction')" to="/dummy-auction">Auction
      </b-nav-item>
    </b-navbar-nav>
    <b-navbar-nav class="ml-auto">
      <slot></slot>
    </b-navbar-nav>
  </b-navbar>
</template>

<script>
export default {
  name: "NavBar",
  data: function () {
    return {}
  }
}
</script>

<style scoped>

</style>
