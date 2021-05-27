import { Switch, Route, Redirect } from 'react-router-dom';

import { withAuth } from '../helpers/withAuth';

import Login from './Login.js';
import Header from './Header.js';
import HomePage from './HomePage.js';
import TokenPage from './TokenPage.js';
import CreateNFT from './CreateNFT.js';
import MyCollection from './MyCollection.js';

import '../styles/App.scss';

const PrivateRoute = ({ component: Component, currentUser, ...rest }) => (
  <Route
    {...rest}
    render={(props) =>
      !currentUser ? (
        <Redirect
          to={{
            pathname: '/login',
            state: { from: props.location },
          }}
        />
      ) : (
        <Component currentUser={currentUser} {...props} />
      )
    }
  />
);

const App = ({ currentUser }) => (
  <div className='App'>
    <Header currentUser={currentUser} />
    <main className='container'>
      <Switch>
        <Route path='/login'>
          <Login currentUser={currentUser} />
        </Route>
        <PrivateRoute
          path='/my-collection'
          currentUser={currentUser}
          component={MyCollection}
        />
        <PrivateRoute
          path='/create-nft'
          currentUser={currentUser}
          component={CreateNFT}
        />
        <PrivateRoute
          path='/:id'
          currentUser={currentUser}
          component={TokenPage}
        />
        <PrivateRoute path='/' currentUser={currentUser} component={HomePage} />
      </Switch>
    </main>
  </div>
);

export default withAuth(App);
