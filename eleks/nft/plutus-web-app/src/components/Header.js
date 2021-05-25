import { withRouter } from 'react-router-dom';

import Nav from 'react-bootstrap/Nav';
import Button from 'react-bootstrap/Button';
import Navbar from 'react-bootstrap/Navbar';

import '../styles/Header.scss';
import { withAuth } from '../helpers/withAuth';

const Header = ({ location, logout }) => (
  <Navbar className='Header'>
    <Navbar.Brand className='brand mr-auto'>NFT case</Navbar.Brand>
    <Nav activeKey={location.pathname}>
      <Nav.Item>
        <Nav.Link href='/'>Home</Nav.Link>
      </Nav.Item>
      <Nav.Item>
        <Nav.Link href='/create-nft'>Create NFT</Nav.Link>
      </Nav.Item>
      <Nav.Item>
        <Nav.Link href='/my-collection'>My collection</Nav.Link>
      </Nav.Item>
    </Nav>
    <Button variant='danger' onClick={logout}>
      Logout
    </Button>
  </Navbar>
);

export default withRouter(withAuth(Header));
