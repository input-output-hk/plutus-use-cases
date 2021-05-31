import { withRouter } from 'react-router-dom';
import { NavLink } from 'react-router-dom';

import Nav from 'react-bootstrap/Nav';
import Card from 'react-bootstrap/Card';
import Button from 'react-bootstrap/Button';
import Navbar from 'react-bootstrap/Navbar';
import NavDropdown from 'react-bootstrap/NavDropdown';

import '../styles/Header.scss';
import { withAuth } from '../helpers/withAuth';

const Header = ({ location, logout, currentUser }) => (
  <Navbar className='Header'>
    <Navbar.Brand className='brand mr-auto'>NFT Marketplace</Navbar.Brand>
    <Nav activeKey={location.pathname}>
      <Nav.Item>
        <NavLink exact className='link' activeClassName='active' to='/'>
          Home
        </NavLink>
      </Nav.Item>
      <Nav.Item>
        <NavLink className='link' activeClassName='active' to='/create-nft'>
          Create NFT
        </NavLink>
      </Nav.Item>
      <Nav.Item>
        <NavLink className='link' activeClassName='active' to='/my-collection'>
          My collection
        </NavLink>
      </Nav.Item>
    </Nav>
    {currentUser && currentUser.id && (
      <div className='user'>
        <Navbar.Text>Signed in as:</Navbar.Text>
        <NavDropdown title={currentUser.name} id='basic-nav-dropdown'>
          <Card.Body>
            <Card.Subtitle className='mb-2 text-muted'>
              Wallet id:
            </Card.Subtitle>
            {currentUser.id}
          </Card.Body>
          <NavDropdown.Divider />
          <Card.Body>
            <Card.Subtitle className='mb-2 text-muted'>
              Wallet public key:
            </Card.Subtitle>
            {currentUser.publicKey}
          </Card.Body>
          <NavDropdown.Divider />
          <Card.Body className='button'>
            <Button variant='danger' onClick={logout}>
              Logout
            </Button>
          </Card.Body>
        </NavDropdown>
      </div>
    )}
  </Navbar>
);

export default withRouter(withAuth(Header));
