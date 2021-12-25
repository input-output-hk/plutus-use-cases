import { Switch, Route, Redirect } from 'react-router-dom';
import { compose } from 'recompose';
import { connect } from 'react-redux';
import { getCurrentUser } from '../reducers';
import Header from './Header.js';
import Login from './Login.js';
import Home from './Home.js';
import Game from './Game.js';

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
    <Header />
    <main className='container'>
      <Switch>
        <Route path='/login'>
          <Login currentUser={currentUser} />
        </Route>
        <PrivateRoute path='/games/:id' currentUser={currentUser} component={Game} />
        <PrivateRoute path='/' currentUser={currentUser} component={Home} />
      </Switch>
    </main>
  </div>
);

const enhancer = compose(
  connect((state) => ({ currentUser: getCurrentUser(state) }), null)
);

export default enhancer(App);
