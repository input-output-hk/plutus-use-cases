# AltLabs web component called alt-swap made in stencil

## To add to a angular project

1. npm install
2. npm install alt-swap --save
3. in app.module.ts, 
change import { NgModule } from '@angular/core'; to import { CUSTOM_ELEMENTS_SCHEMA, NgModule } from '@angular/core';
@NgModule({
  ...
  schemas: [CUSTOM_ELEMENTS_SCHEMA]
  ...
})
4. in main.ts,
add import { defineCustomElements } from 'alt-swap/loader';
and defineCustomElements();
5. and now you can call the component using the tag <alt-swap title="Swap"></alt-swap>

## To add to a rect project

1. npm install
2. npm install alt-swap --save
3. in index.js, 
import { applyPolyfills, defineCustomElements } from 'alt-swap/loader'
applyPolyfills().then(() => {
  defineCustomElements();
});
4. and now you can call the component using the tag <alt-swap title="Swap"></alt-swap>

## To add to a vue project

1. npm install
2. npm install alt-swap --save
3. in main.js,
import { applyPolyfills, defineCustomElements } from 'alt-swap/loader';
applyPolyfills().then(() => {
  defineCustomElements();
});
4. and now you can call the component using the tag <alt-swap title="Swap"></alt-swap>